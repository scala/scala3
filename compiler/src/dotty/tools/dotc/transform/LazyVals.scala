package dotty.tools.dotc
package transform

import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Phases.NeedsCompanions

import scala.collection.mutable
import core._
import Contexts._
import Symbols._
import Decorators._
import NameOps._
import NameKinds._
import StdNames.nme
import rewrite.Rewrites.patch
import util.Positions.Position
import dotty.tools.dotc.transform.MegaPhase._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{untpd, tpd}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Types.{ExprType, NoType, MethodType}
import dotty.tools.dotc.core.Names.Name
import SymUtils._
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.DenotTransformers.{SymTransformer, IdentityDenotTransformer, DenotTransformer}
import Erasure.Boxing.adaptToType

class LazyVals extends MiniPhase with IdentityDenotTransformer {
  import LazyVals._
  import tpd._

  /** this map contains mutable state of transformation: OffsetDefs to be appended to companion object definitions,
    * and number of bits currently used */
  class OffsetInfo(var defs: List[Tree], var ord:Int)
  val appendOffsetDefs = mutable.Map.empty[Symbol, OffsetInfo]

  override def phaseName: String = "lazyVals"

  /** List of names of phases that should have finished processing of tree
    * before this phase starts processing same tree */
  override def runsAfter = Set(classOf[Mixin])

  override def changesMembers = true  // the phase adds lazy val accessors

  def transformer = new LazyVals

  val containerFlags = Flags.Synthetic | Flags.Mutable | Flags.Lazy
  val initFlags      = Flags.Synthetic | Flags.Method

  val containerFlagsMask = Flags.Method | Flags.Lazy | Flags.Accessor | Flags.Module

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context): tpd.Tree =
   transformLazyVal(tree)


  override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context): tpd.Tree = {
    transformLazyVal(tree)
  }

  def transformLazyVal(tree: ValOrDefDef)(implicit ctx: Context): Tree = {
    val sym = tree.symbol
    if (!(sym is Flags.Lazy) ||
        sym.owner.is(Flags.Trait) || // val is accessor, lazy field will be implemented in subclass
        (sym.isStatic && sym.is(Flags.Module, butNot = Flags.Method))) // static module vals are implemented in the JVM by lazy loading
      tree
    else {
      val isField = sym.owner.isClass
      if (isField) {
        if (sym.isVolatile ||
           (sym.is(Flags.Module)/* || ctx.scala2Mode*/) &&
            // TODO assume @volatile once LazyVals uses static helper constructs instead of
            // ones in the companion object.
           !sym.is(Flags.Synthetic))
            // module class is user-defined.
            // Should be threadsafe, to mimic safety guaranteed by global object
          transformMemberDefVolatile(tree)
        else if (sym.is(Flags.Module)) // synthetic module
          transformSyntheticModule(tree)
        else
          transformMemberDefNonVolatile(tree)
      }
      else transformLocalDef(tree)
    }
  }


  /** Append offset fields to companion objects
    */
  override def transformTemplate(template: tpd.Template)(implicit ctx: Context): tpd.Tree = {
    val cls = ctx.owner.asClass

    appendOffsetDefs.get(cls) match {
      case None => template
      case Some(data) =>
        data.defs.foreach(_.symbol.addAnnotation(Annotation(defn.ScalaStaticAnnot)))
        cpy.Template(template)(body = addInFront(data.defs, template.body))
    }

  }

  private def addInFront(prefix: List[Tree], stats: List[Tree]) = stats match {
    case first :: rest if isSuperConstrCall(first) => first :: prefix ::: rest
    case _ => prefix ::: stats
  }

  /** Make an eager val that would implement synthetic module.
    * Eager val ensures thread safety and has less code generated.
    *
    */
  def transformSyntheticModule(tree: ValOrDefDef)(implicit ctx: Context) = {
    val sym = tree.symbol
    val holderSymbol = ctx.newSymbol(sym.owner, LazyLocalName.fresh(sym.asTerm.name),
      Flags.Synthetic, sym.info.widen.resultType).enteredAfter(this)
    val field = ValDef(holderSymbol, tree.rhs.changeOwnerAfter(sym, holderSymbol, this))
    val getter = DefDef(sym.asTerm, ref(holderSymbol))
    Thicket(field, getter)
  }

    /** Replace a local lazy val inside a method,
      * with a LazyHolder from
      * dotty.runtime(eg dotty.runtime.LazyInt)
      */
    def transformLocalDef(x: ValOrDefDef)(implicit ctx: Context) = {
        val valueInitter = x.rhs
        val xname = x.name.asTermName
        val holderName = LazyLocalName.fresh(xname)
        val initName = LazyLocalInitName.fresh(xname)
        val tpe = x.tpe.widen.resultType.widen

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

        val holderSymbol = ctx.newSymbol(x.symbol.owner, holderName, containerFlags, holderImpl.typeRef, coord = x.coord)
        val initSymbol = ctx.newSymbol(x.symbol.owner, initName, initFlags, MethodType(Nil, tpe), coord = x.coord)
        val result = ref(holderSymbol).select(lazyNme.value).withPos(x.pos)
        val flag = ref(holderSymbol).select(lazyNme.initialized)
        val initer = valueInitter.changeOwnerAfter(x.symbol, initSymbol, this)
        val initBody =
          adaptToType(
            ref(holderSymbol).select(defn.Object_synchronized).appliedTo(
              adaptToType(mkNonThreadSafeDef(result, flag, initer), defn.ObjectType)),
            tpe)
        val initTree = DefDef(initSymbol, initBody)
        val holderTree = ValDef(holderSymbol, New(holderImpl.typeRef, List()))
        val methodBody = tpd.If(flag.ensureApplied,
          result.ensureApplied,
          ref(initSymbol).ensureApplied).ensureConforms(tpe)

        val methodTree = DefDef(x.symbol.asTerm, methodBody)
        ctx.debuglog(s"found a lazy val ${x.show},\nrewrote with ${holderTree.show}")
        Thicket(holderTree, initTree, methodTree)
    }


  override def transformStats(trees: List[tpd.Tree])(implicit ctx: Context): List[tpd.Tree] = {
    // backend requires field usage to be after field definition
    // need to bring containers to start of method
    val (holders, stats) =
      trees.partition {
        _.symbol.flags.&~(Flags.Touched) == containerFlags
        // Filtering out Flags.Touched is not required currently, as there are no LazyTypes involved here
        // but just to be more safe
      }
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
      val setFlag = flag.becomes(Literal(Constants.Constant(true)))
      val setTargets = if (isWildcardArg(rhs)) Nil else target.becomes(rhs) :: Nil
      val init = Block(setFlag :: setTargets, target.ensureApplied)
      If(flag.ensureApplied, target.ensureApplied, init)
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
      val setTarget = exp.becomes(rhs)
      val init = Block(List(setTarget), exp)
      If(cond, init, exp)
    }

    def transformMemberDefNonVolatile(x: ValOrDefDef)(implicit ctx: Context) = {
        val claz = x.symbol.owner.asClass
        val tpe = x.tpe.widen.resultType.widen
        assert(!(x.symbol is Flags.Mutable))
        val containerName = LazyLocalName.fresh(x.name.asTermName)
        val containerSymbol = ctx.newSymbol(claz, containerName,
          x.symbol.flags &~ containerFlagsMask | containerFlags | Flags.Private,
          tpe, coord = x.symbol.coord
        ).enteredAfter(this)

        val containerTree = ValDef(containerSymbol, defaultValue(tpe))
        if (x.tpe.isNotNull && tpe <:< defn.ObjectType) { // can use 'null' value instead of flag
          val slowPath = DefDef(x.symbol.asTerm, mkDefNonThreadSafeNonNullable(containerSymbol, x.rhs))
          Thicket(containerTree, slowPath)
        }
        else {
          val flagName = LazyBitMapName.fresh(x.name.asTermName)
          val flagSymbol = ctx.newSymbol(x.symbol.owner, flagName,  containerFlags | Flags.Private, defn.BooleanType).enteredAfter(this)
          val flag = ValDef(flagSymbol, Literal(Constants.Constant(false)))
          val slowPath = DefDef(x.symbol.asTerm, mkNonThreadSafeDef(ref(containerSymbol), ref(flagSymbol), x.rhs))
          Thicket(containerTree, flag, slowPath)
        }
    }

    /** Create a threadsafe lazy accessor equivalent to such code
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
      val flagSymbol = ctx.newSymbol(methodSymbol, lazyNme.flag, containerFlags, defn.LongType)
      val flagDef = ValDef(flagSymbol, Literal(Constant(0L)))

      val thiz = This(claz)(ctx.fresh.setOwner(claz))

      val resultSymbol = ctx.newSymbol(methodSymbol, lazyNme.result, containerFlags, tp)
      val resultDef = ValDef(resultSymbol, defaultValue(tp))

      val retrySymbol = ctx.newSymbol(methodSymbol, lazyNme.retry, containerFlags, defn.BooleanType)
      val retryDef = ValDef(retrySymbol, Literal(Constants.Constant(true)))

      val whileCond = ref(retrySymbol)

      val compute = {
        val handlerSymbol = ctx.newSymbol(methodSymbol, nme.ANON_FUN, Flags.Synthetic,
          MethodType(List(nme.x_1), List(defn.ThrowableType), defn.IntType))
        val caseSymbol = ctx.newSymbol(methodSymbol, nme.DEFAULT_EXCEPTION_NAME, Flags.Synthetic, defn.ThrowableType)
        val triggerRetry = setFlagState.appliedTo(thiz, offset, initState, Literal(Constant(ord)))
        val complete = setFlagState.appliedTo(thiz, offset, computedState, Literal(Constant(ord)))

        val handler = CaseDef(Bind(caseSymbol, ref(caseSymbol)), EmptyTree,
          Block(List(triggerRetry), Throw(ref(caseSymbol))
        ))

        val compute = ref(resultSymbol).becomes(rhs)
        val tr = Try(compute, List(handler), EmptyTree)
        val assign = ref(target).becomes(ref(resultSymbol))
        val noRetry = ref(retrySymbol).becomes(Literal(Constants.Constant(false)))
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
        val noRetry = ref(retrySymbol).becomes(Literal(Constants.Constant(false)))
        val result = ref(resultSymbol).becomes(ref(target))
        val body = Block(noRetry :: result :: Nil, Literal(Constant(())))
        CaseDef(computedState, EmptyTree, body)
      }

      val default = CaseDef(Underscore(defn.LongType), EmptyTree, Literal(Constant(())))

      val cases = Match(stateMask.appliedTo(ref(flagSymbol), Literal(Constant(ord))),
        List(compute, waitFirst, waitSecond, computed, default)) //todo: annotate with @switch

      val whileBody = List(ref(flagSymbol).becomes(getFlag.appliedTo(thiz, offset)), cases)
      val cycle = WhileDo(methodSymbol, whileCond, whileBody)
      DefDef(methodSymbol, Block(resultDef :: retryDef :: flagDef :: cycle :: Nil, ref(resultSymbol)))
    }

    def transformMemberDefVolatile(x: ValOrDefDef)(implicit ctx: Context) = {
        assert(!(x.symbol is Flags.Mutable))

        val tpe = x.tpe.widen.resultType.widen
        val claz = x.symbol.owner.asClass
        val thizClass = Literal(Constant(claz.info))
        val helperModule = ctx.requiredModule("dotty.runtime.LazyVals")
        val getOffset = Select(ref(helperModule), lazyNme.RLazyVals.getOffset)
        var offsetSymbol: TermSymbol = null
        var flag: Tree = EmptyTree
        var ord = 0

        def offsetName(id: Int) = (StdNames.nme.LAZY_FIELD_OFFSET + (if(x.symbol.owner.is(Flags.Module)) "_m_" else "") + id.toString).toTermName

        // compute or create appropriate offsetSymol, bitmap and bits used by current ValDef
        appendOffsetDefs.get(claz) match {
          case Some(info) =>
            val flagsPerLong = (64 / dotty.runtime.LazyVals.BITS_PER_LAZY_VAL).toInt
            info.ord += 1
            ord = info.ord % flagsPerLong
            val id = info.ord / flagsPerLong
            val offsetById = offsetName(id)
            if (ord != 0) { // there are unused bits in already existing flag
              offsetSymbol = claz.info.decl(offsetById)
                .suchThat(sym => (sym is Flags.Synthetic) && sym.isTerm)
                 .symbol.asTerm
            } else { // need to create a new flag
              offsetSymbol = ctx.newSymbol(claz, offsetById, Flags.Synthetic, defn.LongType).enteredAfter(this)
              offsetSymbol.addAnnotation(Annotation(defn.ScalaStaticAnnot))
              val flagName = (StdNames.nme.BITMAP_PREFIX + id.toString).toTermName
              val flagSymbol = ctx.newSymbol(claz, flagName, containerFlags, defn.LongType).enteredAfter(this)
              flag = ValDef(flagSymbol, Literal(Constants.Constant(0L)))
              val offsetTree = ValDef(offsetSymbol, getOffset.appliedTo(thizClass, Literal(Constant(flagName.toString))))
              info.defs = offsetTree :: info.defs
            }

          case None =>
            offsetSymbol = ctx.newSymbol(claz, offsetName(0), Flags.Synthetic, defn.LongType).enteredAfter(this)
            offsetSymbol.addAnnotation(Annotation(defn.ScalaStaticAnnot))
            val flagName = (StdNames.nme.BITMAP_PREFIX + "0").toTermName
            val flagSymbol = ctx.newSymbol(claz, flagName, containerFlags, defn.LongType).enteredAfter(this)
            flag = ValDef(flagSymbol, Literal(Constants.Constant(0L)))
            val offsetTree = ValDef(offsetSymbol, getOffset.appliedTo(thizClass, Literal(Constant(flagName.toString))))
            appendOffsetDefs += (claz -> new OffsetInfo(List(offsetTree), ord))
        }

        val containerName = LazyLocalName.fresh(x.name.asTermName)
        val containerSymbol = ctx.newSymbol(claz, containerName, x.symbol.flags &~ containerFlagsMask | containerFlags, tpe, coord = x.symbol.coord).enteredAfter(this)

        val containerTree = ValDef(containerSymbol, defaultValue(tpe))

        val offset =  ref(offsetSymbol)
        val getFlag = Select(ref(helperModule), lazyNme.RLazyVals.get)
        val setFlag = Select(ref(helperModule), lazyNme.RLazyVals.setFlag)
        val wait =    Select(ref(helperModule), lazyNme.RLazyVals.wait4Notification)
        val state =   Select(ref(helperModule), lazyNme.RLazyVals.state)
        val cas =     Select(ref(helperModule), lazyNme.RLazyVals.cas)

        val accessor = mkThreadSafeDef(x.symbol.asTerm, claz, ord, containerSymbol, x.rhs, tpe, offset, getFlag, state, cas, setFlag, wait)
        if (flag eq EmptyTree)
          Thicket(containerTree, accessor)
        else Thicket(containerTree, flag, accessor)
    }
}

object LazyVals {
  object lazyNme {
    object RLazyVals {
      import dotty.runtime.LazyVals._
      val get               = Names.get.toTermName
      val setFlag           = Names.setFlag.toTermName
      val wait4Notification = Names.wait4Notification.toTermName
      val state             = Names.state.toTermName
      val cas               = Names.cas.toTermName
      val getOffset         = Names.getOffset.toTermName
    }
    val flag        = "flag".toTermName
    val result      = "result".toTermName
    val value       = "value".toTermName
    val initialized = "initialized".toTermName
    val retry       = "retry".toTermName
  }
}



