package dotty.tools.dotc
package transform

import scala.collection.mutable
import core._
import Contexts._
import Symbols._
import Decorators._
import NameOps._
import StdNames.nme
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransformer, MiniPhaseTransform}
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{untpd, tpd}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Types.{ExprType, NoType, MethodType}
import dotty.tools.dotc.core.Names.Name
import dotty.runtime.{LazyVals => RLazyVals} // dotty deviation
import SymUtils._
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.DenotTransformers.{SymTransformer, IdentityDenotTransformer, DenotTransformer}

class LazyVals extends MiniPhaseTransform with SymTransformer {

  import tpd._


  def transformSym(d: SymDenotation)(implicit ctx: Context): SymDenotation = {
    if (d is(Flags.Lazy, butNot = Flags.ModuleVal | Flags.Method)) {
      // Method flag is set on lazy vals coming from Unpickler. They are already methods and shouldn't be transformed twice
      d.copySymDenotation(
        initFlags = d.flags | Flags.Method,
        info = ExprType(d.info))
    }
    else d
  }

  def transformer = new LazyVals

  val containerFlags = Flags.Synthetic | Flags.Mutable | Flags.Lazy
  val initFlags = Flags.Synthetic | Flags.Method

  /** this map contains mutable state of transformation: OffsetDefs to be appended to companion object definitions,
    * and number of bits currently used */
  class OffsetInfo(var defs: List[Tree], var ord:Int)
  val appendOffsetDefs = mutable.Map.empty[Name, OffsetInfo]

  override def phaseName: String = "LazyVals"

    /** List of names of phases that should have finished processing of tree
      * before this phase starts processing same tree */
    // override def ensureAfter: Set[String] = Set("mixin")

    override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (!(tree.mods is Flags.Lazy) || (tree.mods is Flags.ModuleVal)) tree
      else {
        val isField = tree.symbol.owner.isClass

        if (isField) {
          if (tree.symbol.isVolatile) transformFieldValDefVolatile(tree)
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
      case ValDef(name, tpt, _) =>
        val valueInitter = x.rhs
        val holderName = ctx.freshName(name.toString + StdNames.nme.LAZY_LOCAL).toTermName
        val initName = ctx.freshName(name.toString + StdNames.nme.LAZY_LOCAL_INIT).toTermName
        val tpe = x.tpe.widen

        val holderType =
          if (tpe isRef defn.IntClass) "LazyInt"
          else if (tpe isRef defn.LongClass) "LazyLong"
          else if (tpe isRef defn.BooleanClass) "LazyBoolean"
          else if (tpe isRef defn.FloatClass) "LazyFloat"
          else if (tpe isRef defn.DoubleClass) "LazyDouble"
          else if (tpe isRef defn.ByteClass) "LazyByte"
          else if (tpe isRef defn.CharClass) "LazyChar"
          else if (tpe isRef defn.ShortClass) "LazyShort"
          else "LazyRef"


        val holderImpl = ctx.requiredClass("dotty.runtime." + holderType)

        val holderSymbol = ctx.newSymbol(x.symbol.owner, holderName, containerFlags, holderImpl.typeRef, coord = x.pos)
        val initSymbol = ctx.newSymbol(x.symbol.owner, initName, initFlags, MethodType(Nil, tpe), coord = x.pos)
        val result = ref(holderSymbol).select("value".toTermName)
        val flag = ref(holderSymbol).select("initialized".toTermName)
        val initer = valueInitter.changeOwner(x.symbol, initSymbol)
        val initBody =
          ref(holderSymbol).select(defn.Object_synchronized).appliedToType(tpe).appliedTo(
          mkNonThreadSafeDef(result, flag, initer).ensureConforms(tpe))
        val initTree = DefDef(initSymbol, initBody)
        val holderTree = ValDef(holderSymbol, New(holderImpl.typeRef, List()))
        val methodBody = {
          tpd.If(flag, EmptyTree, ref(initSymbol))
          result.ensureConforms(tpe)
          }
        val methodTree = DefDef(x.symbol.asTerm, methodBody)
        ctx.debuglog(s"found a lazy val ${x.show},\n rewrote with ${holderTree.show}")
        Thicket(holderTree, initTree, methodTree)
    }


  override def transformStats(trees: List[tpd.Tree])(implicit ctx: Context, info: TransformerInfo): List[tpd.Tree] = {
    val (holders, stats) = trees.partition { _.symbol.flags == containerFlags}
    holders:::stats
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

    def mkNonThreadSafeDef(target: Tree, flag: Tree, rhs: Tree)(implicit ctx: Context) = {
      val setFlag = Assign(flag, Literal(Constants.Constant(true)))
      val setTarget = Assign(target, rhs)
      val init = Block(List(setFlag, setTarget), target)
      If(flag, target, init)
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
      val cond = ref(target).select(nme.eq).appliedTo(Literal(Constant(null)))
      val exp = ref(target)
      val setTarget = Assign(exp, rhs)
      val init = Block(List(setTarget), exp)
      If(cond, init, exp)
    }

    def transformFieldValDefNonVolatile(x: ValDef)(implicit ctx: Context) = x match {
      case ValDef(name, tpt, _) if (x.mods is Flags.Lazy) =>
        val claz = x.symbol.owner.asClass
        val tpe = x.tpe.widen
        assert(!(x.mods is Flags.Mutable))
        val containerName = ctx.freshName(name.toString + StdNames.nme.LAZY_LOCAL).toTermName
        val containerSymbol = ctx.newSymbol(claz, containerName, (x.mods &~ Flags.Lazy | containerFlags).flags, tpe, coord = x.symbol.coord).enteredAfter(this)

        val containerTree = ValDef(containerSymbol, initValue(tpe))
        if (x.tpe.isNotNull && tpe <:< defn.AnyRefType) { // can use 'null' value instead of flag
          val slowPath = DefDef(x.symbol.asTerm, mkDefNonThreadSafeNonNullable(containerSymbol, x.rhs))
          Thicket(List(containerTree, slowPath))
        }
        else {
          val flagName = ctx.freshName(name.toString + StdNames.nme.BITMAP_PREFIX).toTermName
          val flagSymbol = ctx.newSymbol(x.symbol.owner, flagName,  containerFlags, defn.BooleanType)
          val flag = ValDef(flagSymbol, Literal(Constants.Constant(false)))
          val slowPath = DefDef(x.symbol.asTerm, mkNonThreadSafeDef(ref(containerSymbol), ref(flagSymbol), x.rhs))
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
      * FIXME: Don't use strings with toTermName, use predefined names instead.
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
      val resultDef = ValDef(resultSymbol, initValue(tp))

      val retrySymbol = ctx.newSymbol(methodSymbol, "retry".toTermName, containerFlags, defn.BooleanType)
      val retryDef = ValDef(retrySymbol, Literal(Constants.Constant(true)))

      val whileCond = ref(retrySymbol)

      val compute = {
        val handlerSymbol = ctx.newSymbol(methodSymbol, "$anonfun".toTermName, Flags.Synthetic,
          MethodType(List("x$1".toTermName), List(defn.ThrowableType), defn.IntType))
        val caseSymbol = ctx.newSymbol(methodSymbol, nme.DEFAULT_EXCEPTION_NAME, Flags.Synthetic, defn.ThrowableType)
        val complete = setFlagState.appliedTo(thiz, offset, initState, Literal(Constant(ord)))
        val handler = CaseDef(Bind(caseSymbol, ref(caseSymbol)), EmptyTree,
          Block(List(complete), Throw(ref(caseSymbol))
        ))

        val compute = Assign(ref(resultSymbol), rhs)
        val tr = Try(compute, List(handler), EmptyTree)
        val assign = Assign(ref(target), ref(resultSymbol))
        val noRetry = Assign(ref(retrySymbol), Literal(Constants.Constant(false)))
        val body = If(casFlag.appliedTo(thiz, offset, ref(flagSymbol), computeState, Literal(Constant(ord))),
          Block(tr :: assign :: complete :: noRetry :: Nil, Literal(Constant(()))),
          Literal(Constant(())))

        CaseDef(initState, EmptyTree, body)
      }

      val waitFirst = {
        val wait = waitOnLock.appliedTo(thiz, offset, ref(flagSymbol), Literal(Constant(ord)))
        CaseDef(computeState, EmptyTree, wait)
      }

      val waitSecond = {
        val wait = waitOnLock.appliedTo(thiz, offset, ref(flagSymbol), Literal(Constant(ord)))
        CaseDef(notifyState, EmptyTree, wait)
      }

      val computed = {
        val noRetry = Assign(ref(retrySymbol), Literal(Constants.Constant(false)))
        val result = Assign(ref(resultSymbol), ref(target))
        val body = Block(noRetry :: result :: Nil, Literal(Constant(())))
        CaseDef(computedState, EmptyTree, body)
      }

      val cases = Match(stateMask.appliedTo(ref(flagSymbol), Literal(Constant(ord))),
        List(compute, waitFirst, waitSecond, computed)) //todo: annotate with @switch

      val whileBody = Block(List(Assign(ref(flagSymbol), getFlag.appliedTo(thiz, offset))), cases)
      val cycle = untpd.WhileDo(whileCond, whileBody).withTypeUnchecked(defn.UnitType)
      DefDef(methodSymbol, Block(resultDef :: retryDef :: flagDef :: cycle :: Nil, ref(resultSymbol)))
    }

    def transformFieldValDefVolatile(x: ValDef)(implicit ctx: Context) = x match {
      case ValDef(name, tpt, _) if (x.mods is Flags.Lazy) =>
        assert(!(x.mods is Flags.Mutable))

        val tpe = x.tpe.widen
        val claz = x.symbol.owner.asClass
        val thiz = This(claz)(ctx.fresh.setOwner(claz))
        val companion = claz.companionModule
        val helperModule = ctx.requiredModule("dotty.runtime.LazyVals")
        val getOffset = Select(ref(helperModule), RLazyVals.Names.getOffset.toTermName)
        var offsetSymbol: TermSymbol = null
        var flag: Tree = EmptyTree
        var ord = 0

        // compute or create appropriate offsetSymol, bitmap and bits used by current ValDef
        appendOffsetDefs.get(companion.name.moduleClassName) match {
          case Some(info) =>
            val flagsPerLong = 64 / RLazyVals.BITS_PER_LAZY_VAL
            info.ord += 1
            ord = info.ord % flagsPerLong
            val id = info.ord / flagsPerLong
            if (ord != 0) { // there are unused bits in already existing flag
              offsetSymbol = companion.moduleClass.info.decl((StdNames.nme.LAZY_FIELD_OFFSET + id.toString).toTermName)
                .suchThat(sym => (sym is Flags.Synthetic) && sym.isTerm)
                 .symbol.asTerm
            } else { // need to create a new flag
              offsetSymbol = ctx.newSymbol(companion.moduleClass, (StdNames.nme.LAZY_FIELD_OFFSET + id.toString).toTermName, Flags.Synthetic, defn.LongType).enteredAfter(this)
              val flagName = (StdNames.nme.BITMAP_PREFIX + id.toString).toTermName
              val flagSymbol = ctx.newSymbol(claz, flagName, containerFlags, defn.LongType).enteredAfter(this)
              flag = ValDef(flagSymbol, Literal(Constants.Constant(0L)))
              val offsetTree = ValDef(offsetSymbol, getOffset.appliedTo(thiz, Literal(Constant(flagName.toString))))
              info.defs = offsetTree :: info.defs
            }

          case None =>
            offsetSymbol = ctx.newSymbol(companion.moduleClass, (StdNames.nme.LAZY_FIELD_OFFSET + "0").toTermName, Flags.Synthetic, defn.LongType).enteredAfter(this)
            val flagName = (StdNames.nme.BITMAP_PREFIX + "0").toTermName
            val flagSymbol = ctx.newSymbol(claz, flagName, containerFlags, defn.LongType).enteredAfter(this)
            flag = ValDef(flagSymbol, Literal(Constants.Constant(0L)))
            val offsetTree = ValDef(offsetSymbol, getOffset.appliedTo(thiz, Literal(Constant(flagName.toString))))
            appendOffsetDefs += (companion.name.moduleClassName -> new OffsetInfo(List(offsetTree), ord))
        }

        val containerName = ctx.freshName(name.toString + StdNames.nme.LAZY_LOCAL).toTermName
        val containerSymbol = ctx.newSymbol(claz, containerName, (x.mods &~ Flags.Lazy | containerFlags).flags, tpe, coord = x.symbol.coord).enteredAfter(this)
        val containerTree = ValDef(containerSymbol, initValue(tpe))

        val offset = Select(ref(companion), offsetSymbol.name)
        val getFlag = Select(ref(helperModule), RLazyVals.Names.get.toTermName)
        val setFlag = Select(ref(helperModule), RLazyVals.Names.setFlag.toTermName)
        val wait = Select(ref(helperModule), RLazyVals.Names.wait4Notification.toTermName)
        val state = Select(ref(helperModule), RLazyVals.Names.state.toTermName)
        val cas = Select(ref(helperModule), RLazyVals.Names.cas.toTermName)

        val accessor = mkThreadSafeDef(x.symbol.asTerm, claz, ord, containerSymbol, x.rhs, tpe, offset, getFlag, state, cas, setFlag, wait)
        if (flag eq EmptyTree)
          Thicket(List(containerTree, accessor))
        else Thicket(List(containerTree, flag, accessor))
    }
}



