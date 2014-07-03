package dotty.tools.dotc
package transform

import scala.collection.mutable
import core._
import Contexts._
import Symbols._
import Decorators._
import NameOps._
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransformer, TreeTransform}
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{untpd, tpd}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.core.Names.Name
import dotty.runtime.LazyVals
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer

class LazyValTranformContext {

  import tpd._


  def transformer = new LazyValsTransform

  val containerFlags = Flags.Synthetic | Flags.Mutable

  /** this map contains mutable state of transformation: OffsetDefs to be appended to companion object definitions,
    * and number of bits currently used */
  class OffsetInfo(var defs: List[Tree], var ord:Int)
  val appendOffsetDefs = mutable.Map.empty[Name, OffsetInfo]

  val infoTransformerNewDefinitions = mutable.Map.empty[ClassSymbol, ListBuffer[Symbol]]

  def addSym(owner: ClassSymbol, sym: Symbol) = {
    infoTransformerNewDefinitions.get(owner) match {
      case Some(x) => x += sym
      case None => infoTransformerNewDefinitions.put(owner, ListBuffer(sym))
    }
  }

  class LazyValsTransform extends TreeTransform with DenotTransformer {

    override def name: String = "LazyVals"

    /** List of names of phases that should have finished their processing of all compilation units
      * before this phase starts */

    /** List of names of phases that should have finished their processing of all compilation units
      * before this phase starts */

    /** List of names of phases that should have finished processing of tree
      * before this phase starts processing same tree */
    // override def ensureAfter: Set[String] = Set("mixin")

    def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = {
      ref match {
        case ref: SymDenotation if ref.symbol.isClass =>
          val oldSym = ref.symbol.asClass
          infoTransformerNewDefinitions.get(oldSym) match {
            case Some(x) =>
              val den = ref.copySymDenotation()
              den.resetFlag(Flags.Frozen)
              x.foreach(stat => den.asClass.enter(stat))
              den
            case None =>
              ref
          }
        case _ => ref
      }
    }

    override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (!(tree.mods is Flags.Lazy)) tree
      else {
        val isField = tree.symbol.owner.isClass
        val isVolatile = tree.symbol.hasAnnotation(defn.VolatileAnnot)

        if (isField) {
          if (isVolatile) transformFieldValDefVolatile(tree)
          else transformFieldValDefNonVolatile(tree)
        }
        else transformLocalValDef(tree)
      }
    }

    /** Append offset fields to companion objects
     */
    override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (!tree.symbol.isClass) tree
      else {
        appendOffsetDefs.get(tree.symbol.name) match {
          case None => tree
          case Some(data) =>
            val template = tree.rhs.asInstanceOf[Template]
            ClassDef(tree.symbol.asClass, template.constr, data.defs.mapConserve(transformFollowingDeep) ::: template.body)
        }
      }
    }
    /** Replace a local lazy val inside a method,
      * with a LazyHolder from
      * dotty.runtime(eg dotty.runtime.LazyInt)
      */
    def transformLocalValDef(x: ValDef)(implicit ctx: Context) = x match {
      case x@ValDef(mods, name, tpt, rhs) =>
        val valueInitter = rhs
        val holderName = ctx.freshName(name.toString + StdNames.nme.LAZY_LOCAL).toTermName
        val tpe = x.tpe.widen

        val holderType =
          if (tpe =:= defn.IntType) "LazyInt"
          else if (tpe =:= defn.LongType) "LazyLong"
          else if (tpe =:= defn.BooleanType) "LazyBoolean"
          else if (tpe =:= defn.FloatType) "LazyFloat"
          else if (tpe =:= defn.DoubleType) "LazyDouble"
          else if (tpe =:= defn.ByteType) "LazyByte"
          else if (tpe =:= defn.CharType) "LazyChar"
          else if (tpe =:= defn.ShortType) "LazyShort"
          else "LazyRef"

        val holderImpl = ctx.requiredClass("dotty.runtime." + holderType)

        val holderSymbol = ctx.newSymbol(x.symbol.owner, holderName, containerFlags, holderImpl.typeRef, coord = x.symbol.coord)
        val holderTree = ValDef(holderSymbol, New(holderImpl.typeRef, List(valueInitter)))
        val methodBody =
          if(holderType != "LazyRef") Select(Ident(holderSymbol.termRef), "value".toTermName)
          else TypeApply(Select(Select(Ident(holderSymbol.termRef), "value".toTermName), defn.Any_asInstanceOf), List(TypeTree(tpe)))
        val methodTree = DefDef(x.symbol.asTerm, methodBody)
        ctx.debuglog(s"found a lazy val ${x.show},\n rewrote with ${holderTree.show}")
        Thicket(holderTree, methodTree)
    }

    /** Create non-threadsafe lazy accessor equivalent to such code
      * def methodSymbol() = {
      *   if (flag) target
      *   else {
      *     target = rhs
      *     flag = true
      *     target
      *     }
      *   }
      */

    def mkNonThreadSafeDef(target: Symbol, flag: Symbol, rhs: Tree)(implicit ctx: Context) = {
      val cond = Ident(flag.termRef)
      val exp = Ident(target.termRef)
      val setFlag = Assign(cond, Literal(Constants.Constant(true)))
      val setTarget = Assign(exp, rhs)
      val init = Block(List(setFlag, setTarget), exp)
      If(cond, exp, init)
    }

    /** Create non-threadsafe lazy accessor for not-nullable types  equivalent to such code
      * def methodSymbol() = {
      *   if (target eq null) {
      *     target = rhs
      *     target
      *   } else target
      * }
      */
    def mkDefNonThreadSafeNonNullable(target: Symbol, rhs: Tree)(implicit ctx: Context) = {
      val cond = Apply(Select(Ident(target.termRef), "eq".toTermName), List(Literal(Constant(null))))
      val exp = Ident(target.termRef)
      val setTarget = Assign(exp, rhs)
      val init = Block(List(setTarget), exp)
      If(cond, init, exp)
    }

    def initValue(tpe: Types.Type)(implicit ctx: Context) =
      if (tpe =:= defn.IntType) Constant(0)
      else if (tpe =:= defn.LongType) Constant(0L)
      else if (tpe =:= defn.BooleanType) Constant(false)
      else if (tpe =:= defn.CharType)  Constant('\u0000')
      else if (tpe =:= defn.FloatType) Constant(0f)
      else if (tpe =:= defn.DoubleType) Constant(0d)
      else if (tpe =:= defn.ByteType) Constant(0.toByte)
      else if (tpe =:= defn.ShortType) Constant(0.toShort)
      else Constant(null)


    def transformFieldValDefNonVolatile(x: ValDef)(implicit ctx: Context) = x match {
      case x@ValDef(mods, name, tpt, rhs) if (mods is Flags.Lazy) =>
        val claz = x.symbol.owner.asClass
        val tpe = x.tpe.widen
        assert(!(mods is Flags.Mutable))
        val containerName = ctx.freshName(name.toString + StdNames.nme.LAZY_LOCAL).toTermName
        val containerSymbol = ctx.newSymbol(claz, containerName, (mods &~ Flags.Lazy | containerFlags).flags, tpe, coord = x.symbol.coord)
        addSym(claz, containerSymbol)

        val containerTree = ValDef(containerSymbol, Literal(initValue(tpe)))
        if (x.tpe.isNotNull && initValue(tpe).tag == Constants.NullTag) {
          val slowPath = DefDef(x.symbol.asTerm, mkDefNonThreadSafeNonNullable(containerSymbol, rhs))
          Thicket(List(containerTree, slowPath))
        }
        else {
          val flagName = ctx.freshName(name.toString + StdNames.nme.BITMAP_PREFIX).toTermName
          val flagSymbol = ctx.newSymbol(x.symbol.owner, flagName,  containerFlags, defn.BooleanType)
          val flag = ValDef(flagSymbol, Literal(Constants.Constant(false)))
          val slowPath = DefDef(x.symbol.asTerm, mkNonThreadSafeDef(containerSymbol, flagSymbol, rhs))
          Thicket(List(containerTree, flag, slowPath))
        }

    }

    /** Create non-threadsafe lazy accessor equivalent to such code
      *
      * def methodSymbol(): Int = {
      *   val result: Int = 0
      *   val retry: Boolean = true
      *   var flag: Long = 0L
      *   while retry do {
      *     flag = dotty.runtime.LazyVals.get(this, $claz.$OFFSET)
      *     dotty.runtime.LazyVals.STATE(flag, 0) match {
      *       case 0 =>
      *         if dotty.runtime.LazyVals.CAS(this, $claz.$OFFSET, flag, 1, $ord) {
      *           try {result = rhs} catch {
      *             case x: Throwable =>
      *               dotty.runtime.LazyVals.setFlag(this, $claz.$OFFSET, 0, $ord)
      *               throw x
      *           }
      *           $target = result
      *           dotty.runtime.LazyVals.setFlag(this, $claz.$OFFSET, 3, $ord)
      *           retry = false
      *           }
      *       case 1 =>
      *         dotty.runtime.LazyVals.wait4Notification(this, $claz.$OFFSET, flag, $ord)
      *       case 2 =>
      *         dotty.runtime.LazyVals.wait4Notification(this, $claz.$OFFSET, flag, $ord)
      *       case 3 =>
      *         retry = false
      *         result = $target
      *       }
      *     }
      *   result
      * }
      */
    def mkThreadSafeDef(methodSymbol: TermSymbol, claz: ClassSymbol, ord: Int, target: Symbol, rhs: Tree, tp: Types.Type, offset: Tree, getFlag: Tree, stateMask: Tree, casFlag: Tree, setFlagState: Tree, waitOnLock: Tree)(implicit ctx: Context) = {
      val initState = Literal(Constants.Constant(0))
      val computeState = Literal(Constants.Constant(1))
      val notifyState = Literal(Constants.Constant(2))
      val computedState = Literal(Constants.Constant(3))
      val flagSymbol = ctx.newSymbol(methodSymbol, "flag".toTermName, containerFlags, defn.LongType)
      val flagDef = ValDef(flagSymbol, Literal(Constant(0L)))

      val thiz = This(claz)(ctx.fresh.setOwner(claz))

      val resultSymbol = ctx.newSymbol(methodSymbol, "result".toTermName, containerFlags, tp)
      val resultDef = ValDef(resultSymbol, Literal(initValue(tp.widen)))

      val retrySymbol = ctx.newSymbol(methodSymbol, "retry".toTermName, containerFlags, defn.BooleanType)
      val retryDef = ValDef(retrySymbol, Literal(Constants.Constant(true)))

      val whileCond = Ident(retrySymbol.termRef)

      val compute = {
        val handlerSymbol = ctx.newSymbol(methodSymbol, "$anonfun".toTermName, Flags.Synthetic,
          MethodType(List("x$1".toTermName), List(defn.ThrowableType), defn.IntType))

        val handler = Closure(handlerSymbol, {
          args =>
            val exception = args.head.head
            val complete = Apply(setFlagState, List(thiz, offset, initState, Literal(Constant(ord))))
            Block(List(complete), Throw(exception))
        })

        val compute = Assign(Ident(resultSymbol.termRef), rhs)
        val tr = Try(compute, handler, EmptyTree)
        val assign = Assign(Ident(target.termRef), Ident(resultSymbol.termRef))
        val complete = Apply(setFlagState, List(thiz, offset, computedState, Literal(Constant(ord))))
        val noRetry = Assign(Ident(retrySymbol.termRef), Literal(Constants.Constant(false)))
        val body = If(Apply(casFlag, List(thiz, offset, Ident(flagSymbol.termRef), computeState, Literal(Constant(ord)))),
          Block(tr :: assign :: complete :: noRetry :: Nil, Literal(Constant(()))),
          Literal(Constant(())))

        CaseDef(initState, EmptyTree, body)
      }

      val waitFirst = {
        val wait = Apply(waitOnLock, List(thiz, offset, Ident(flagSymbol.termRef), Literal(Constant(ord))))
        CaseDef(computeState, EmptyTree, wait)
      }

      val waitSecond = {
        val wait = Apply(waitOnLock, List(thiz, offset, Ident(flagSymbol.termRef), Literal(Constant(ord))))
        CaseDef(notifyState, EmptyTree, wait)
      }

      val computed = {
        val noRetry = Assign(Ident(retrySymbol.termRef), Literal(Constants.Constant(false)))
        val result = Assign(Ident(resultSymbol.termRef), Ident(target.termRef))
        val body = Block(noRetry :: result :: Nil, Literal(Constant(())))
        CaseDef(computedState, EmptyTree, body)
      }

      val cases = Match(Apply(stateMask, List(Ident(flagSymbol.termRef), Literal(Constant(ord)))),
        List(compute, waitFirst, waitSecond, computed)) //todo: annotate with @switch

      val whileBody = Block(List(Assign(Ident(flagSymbol.termRef), Apply(getFlag, List(thiz, offset)))), cases)
      val cycle = untpd.WhileDo(whileCond, whileBody).withTypeUnchecked(defn.UnitType)
      DefDef(methodSymbol, Block(resultDef :: retryDef :: flagDef :: cycle :: Nil, Ident(resultSymbol.termRef)))
    }

    def transformFieldValDefVolatile(x: ValDef)(implicit ctx: Context) = x match {
      case x@ValDef(mods, name, tpt, rhs) if (mods is Flags.Lazy) =>
        assert(!(mods is Flags.Mutable))

        val tpe = x.tpe.widen
        val claz = x.symbol.owner.asClass
        val thiz = This(claz)(ctx.fresh.setOwner(claz))
        val companion = claz.companionModule
        val helperModule = ctx.requiredModule("dotty.runtime.LazyVals")
        val getOffset = Select(Ident(helperModule.termRef), LazyVals.Names.getOffset.toTermName)
        var offsetSymbol: TermSymbol = null
        var flag: Tree = EmptyTree
        var ord = 0

        // compute or create appropriate offsetSymol, bitmap and bits used by current ValDef
        appendOffsetDefs.get(companion.name.moduleClassName) match {
          case Some(info) =>
            val flagsPerLong = 64 / LazyVals.BITS_PER_LAZY_VAL
            info.ord += 1
            ord = info.ord % flagsPerLong
            val id = info.ord / flagsPerLong
            if(ord != 0) { // there are unused bits in already existing flag
              offsetSymbol = companion.moduleClass.info.decl((StdNames.nme.LAZY_FIELD_OFFSET + id.toString).toTermName)
                .suchThat(sym => (sym is Flags.Synthetic) && sym.isTerm)
                 .symbol.asTerm
            } else { // need to create a new flag
              offsetSymbol = ctx.newSymbol(companion.moduleClass, (StdNames.nme.LAZY_FIELD_OFFSET + id.toString).toTermName, Flags.Synthetic, defn.LongType).entered
              val flagName = (StdNames.nme.BITMAP_PREFIX + id.toString).toTermName
              val flagSymbol = ctx.newSymbol(claz, flagName, containerFlags, defn.LongType)
              addSym(claz, flagSymbol)
              flag = ValDef(flagSymbol, Literal(Constants.Constant(0L)))
              val offsetTree = ValDef(offsetSymbol, Apply(getOffset, List(thiz, Literal(Constant(flagName.toString)))))
              info.defs = offsetTree :: info.defs
            }

          case None =>
            offsetSymbol = ctx.newSymbol(companion.moduleClass, (StdNames.nme.LAZY_FIELD_OFFSET + "0").toTermName, Flags.Synthetic, defn.LongType).entered
            val flagName = (StdNames.nme.BITMAP_PREFIX + "0").toTermName
            val flagSymbol = ctx.newSymbol(claz, flagName, containerFlags, defn.LongType)
            addSym(claz, flagSymbol)
            flag = ValDef(flagSymbol, Literal(Constants.Constant(0L)))
            val offsetTree = ValDef(offsetSymbol, Apply(getOffset, List(thiz, Literal(Constant(flagName.toString)))))
            appendOffsetDefs += (companion.name.moduleClassName -> new OffsetInfo(List(offsetTree), ord))
        }

        val containerName = ctx.freshName(name.toString + StdNames.nme.LAZY_LOCAL).toTermName
        val containerSymbol = ctx.newSymbol(claz, containerName, (mods &~ Flags.Lazy | containerFlags).flags, tpe, coord = x.symbol.coord)
        addSym(claz, containerSymbol)
        val containerTree = ValDef(containerSymbol, Literal(initValue(tpe)))

        val offset = Select(Ident(companion.termRef), offsetSymbol.name)
        val getFlag = Select(Ident(helperModule.termRef), LazyVals.Names.get.toTermName)
        val setFlag = Select(Ident(helperModule.termRef), LazyVals.Names.setFlag.toTermName)
        val wait = Select(Ident(helperModule.termRef), LazyVals.Names.wait4Notification.toTermName)
        val state = Select(Ident(helperModule.termRef), LazyVals.Names.state.toTermName)
        val cas = Select(Ident(helperModule.termRef), LazyVals.Names.cas.toTermName)

        val accessor = mkThreadSafeDef(x.symbol.asTerm, claz, ord, containerSymbol, rhs, tpe, offset, getFlag, state, cas, setFlag, wait)
        if(flag eq EmptyTree)
          Thicket(List(containerTree, accessor))
        else Thicket(List(containerTree, flag, accessor))
    }

  }
}



