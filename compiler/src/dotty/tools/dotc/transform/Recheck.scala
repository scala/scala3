package dotty.tools
package dotc
package transform

import core.*
import Symbols.*, Contexts.*, Types.*, ContextOps.*, Decorators.*, SymDenotations.*
import Flags.*, SymUtils.*, NameKinds.*
import Phases.Phase
import DenotTransformers.IdentityDenotTransformer
import NamerOps.{methodType, linkConstructorParams}
import NullOpsDecorator.stripNull
import typer.ErrorReporting.err
import ast.*
import typer.ProtoTypes.*
import config.Printers.recheckr
import util.Property
import StdNames.nme
import reporting.trace

abstract class Recheck extends Phase, IdentityDenotTransformer:
  thisPhase =>

  import ast.tpd.*

  def preRecheckPhase = this.prev.asInstanceOf[PreRecheck]

  override def isEnabled(using Context) = ctx.settings.Yrecheck.value
  override def changesBaseTypes: Boolean = true

  override def isCheckable = false
    // TODO: investigate what goes wrong we Ycheck directly after rechecking.
    // One failing test is pos/i583a.scala

  def run(using Context): Unit =
    val unit = ctx.compilationUnit
    //println(i"recheck types of $unit")
    newRechecker().check()

  def newRechecker()(using Context): Rechecker

  class Rechecker(ictx: Context):
    val ta = ictx.typeAssigner

    extension (sym: Symbol) def updateInfo(newInfo: Type)(using Context): Unit =
      if sym.info ne newInfo then
        sym.copySymDenotation().installAfter(thisPhase) // reset
        sym.copySymDenotation(
            info = newInfo,
            initFlags =
              if newInfo.isInstanceOf[LazyType] then sym.flags &~ Touched
              else sym.flags
          ).installAfter(preRecheckPhase)

    /** Hook to be overridden */
    protected def reinfer(tp: Type)(using Context): Type = tp

    def reinferResult(info: Type)(using Context): Type = info match
      case info: MethodOrPoly =>
        info.derivedLambdaType(resType = reinferResult(info.resultType))
      case _ =>
        reinfer(info)

    def enterDef(stat: Tree)(using Context): Unit =
      val sym = stat.symbol
      stat match
        case stat: ValOrDefDef if stat.tpt.isInstanceOf[InferredTypeTree] =>
          sym.updateInfo(reinferResult(sym.info))
        case stat: Bind =>
          sym.updateInfo(reinferResult(sym.info))
        case _ =>

    def recheckIdent(tree: Ident)(using Context): Type =
      tree.tpe

    /** Keep the symbol of the `select` but re-infer its type */
    def recheckSelect(tree: Select)(using Context): Type = tree match
      case Select(qual, name) =>
        val qualType = recheck(qual).widenIfUnstable
        if name.is(OuterSelectName) then tree.tpe
        else
          //val pre = ta.maybeSkolemizePrefix(qualType, name)
          val mbr = qualType.findMember(name, qualType,
              excluded = if tree.symbol.is(Private) then EmptyFlags else Private
            ).suchThat(tree.symbol ==)
          qualType.select(name, mbr)

    def recheckBind(tree: Bind, pt: Type)(using Context): Type = tree match
      case Bind(name, body) =>
        enterDef(tree)
        val bodyType = recheck(body, pt)
        val sym = tree.symbol
        if sym.isType then sym.typeRef else sym.info

    def recheckLabeled(tree: Labeled, pt: Type)(using Context): Type = tree match
      case Labeled(bind, expr) =>
        val bindType = recheck(bind, pt)
        val exprType = recheck(expr, defn.UnitType)
        bindType

    def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Type =
      if !tree.rhs.isEmpty then recheck(tree.rhs, tree.symbol.info)
      sym.termRef

    def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      tree.paramss.foreach(_.foreach(enterDef))
      val rhsCtx = linkConstructorParams(sym)
      if !tree.rhs.isEmpty && !sym.isInlineMethod && !sym.isEffectivelyErased then
        recheck(tree.rhs, tree.symbol.localReturnType)(using rhsCtx)
      sym.termRef

    def recheckTypeDef(tree: TypeDef, sym: Symbol)(using Context): Type =
      recheck(tree.rhs)
      sym.typeRef

    def recheckClassDef(tree: TypeDef, impl: Template, sym: ClassSymbol)(using Context): Type =
      recheck(impl.constr)
      impl.parentsOrDerived.foreach(recheck(_))
      recheck(impl.self)
      recheckStats(impl.body)
      sym.typeRef

    // Need to remap Object to FromJavaObject since it got lost in ElimRepeated
    private def mapJavaArgs(formals: List[Type])(using Context): List[Type] =
      val tm = new TypeMap:
        def apply(t: Type) = t match
          case t: TypeRef if t.symbol == defn.ObjectClass => defn.FromJavaObjectType
          case _ => mapOver(t)
      formals.mapConserve(tm)

    def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      recheck(tree.fun).widen match
        case fntpe: MethodType =>
          assert(sameLength(fntpe.paramInfos, tree.args))
          val formals =
            if tree.symbol.is(JavaDefined) then mapJavaArgs(fntpe.paramInfos)
            else fntpe.paramInfos
          def recheckArgs(args: List[Tree], formals: List[Type], prefs: List[ParamRef]): List[Type] = args match
            case arg :: args1 =>
              val argType = recheck(arg, formals.head)
              val formals1 =
                if fntpe.isParamDependent
                then formals.tail.map(_.substParam(prefs.head, argType))
                else formals.tail
              argType :: recheckArgs(args1, formals1, prefs.tail)
            case Nil =>
              assert(formals.isEmpty)
              Nil
          val argTypes = recheckArgs(tree.args, formals, fntpe.paramRefs)
          fntpe.instantiate(argTypes)

    def recheckTypeApply(tree: TypeApply, pt: Type)(using Context): Type =
      recheck(tree.fun).widen match
        case fntpe: PolyType =>
          assert(sameLength(fntpe.paramInfos, tree.args))
          val argTypes = tree.args.map(recheck(_))
          fntpe.instantiate(argTypes)

    def recheckTyped(tree: Typed)(using Context): Type =
      val tptType = recheck(tree.tpt)
      recheck(tree.expr, tptType)
      tptType

    def recheckAssign(tree: Assign)(using Context): Type =
      val lhsType = recheck(tree.lhs)
      recheck(tree.rhs, lhsType.widen)
      defn.UnitType

    def recheckBlock(stats: List[Tree], expr: Tree, pt: Type)(using Context): Type =
      recheckStats(stats)
      val exprType = recheck(expr, pt.dropIfProto)
      TypeOps.avoid(exprType, localSyms(stats).filterConserve(_.isTerm))

    def recheckBlock(tree: Block, pt: Type)(using Context): Type =
      recheckBlock(tree.stats, tree.expr, pt)

    def recheckInlined(tree: Inlined, pt: Type)(using Context): Type =
      recheckBlock(tree.bindings, tree.expansion, pt)

    def recheckIf(tree: If, pt: Type)(using Context): Type =
      recheck(tree.cond, defn.BooleanType)
      recheck(tree.thenp, pt) | recheck(tree.elsep, pt)

    def recheckClosure(tree: Closure, pt: Type)(using Context): Type =
      if tree.tpt.isEmpty then
        tree.meth.tpe.widen.toFunctionType(tree.meth.symbol.is(JavaDefined))
      else
        recheck(tree.tpt)

    def recheckMatch(tree: Match, pt: Type)(using Context): Type =
      val selectorType = recheck(tree.selector)
      val casesTypes = tree.cases.map(recheck(_, selectorType.widen, pt))
      TypeComparer.lub(casesTypes)

    def recheck(tree: CaseDef, selType: Type, pt: Type)(using Context): Type =
      recheck(tree.pat, selType)
      recheck(tree.guard, defn.BooleanType)
      recheck(tree.body, pt)

    def recheckReturn(tree: Return)(using Context): Type =
      recheck(tree.expr, tree.from.symbol.returnProto)
      defn.NothingType

    def recheckWhileDo(tree: WhileDo)(using Context): Type =
      recheck(tree.cond, defn.BooleanType)
      recheck(tree.body, defn.UnitType)
      defn.UnitType

    def recheckTry(tree: Try, pt: Type)(using Context): Type =
      val bodyType = recheck(tree.expr, pt)
      val casesTypes = tree.cases.map(recheck(_, defn.ThrowableType, pt))
      val finalizerType = recheck(tree.finalizer, defn.UnitType)
      TypeComparer.lub(bodyType :: casesTypes)

    def recheckSeqLiteral(tree: SeqLiteral, pt: Type)(using Context): Type =
      val elemProto = pt.stripNull.elemType match
        case NoType => WildcardType
        case bounds: TypeBounds => WildcardType(bounds)
        case elemtp => elemtp
      val declaredElemType = recheck(tree.elemtpt)
      val elemTypes = tree.elems.map(recheck(_, elemProto))
      TypeComparer.lub(declaredElemType :: elemTypes)

    def recheckTypeTree(tree: TypeTree)(using Context): Type = tree match
      case tree: InferredTypeTree => reinfer(tree.tpe)
      case _ => tree.tpe

    def recheckAnnotated(tree: Annotated)(using Context): Type =
      tree.tpe match
        case tp: AnnotatedType =>
          val argType = recheck(tree.arg)
          tp.derivedAnnotatedType(argType, tp.annot)

    def recheckAlternative(tree: Alternative, pt: Type)(using Context): Type =
      val altTypes = tree.trees.map(recheck(_, pt))
      TypeComparer.lub(altTypes)

    def recheckPackageDef(tree: PackageDef)(using Context): Type =
      recheckStats(tree.stats)
      NoType

    def recheckStats(stats: List[Tree])(using Context): Unit =
      stats.foreach(enterDef)
      stats.foreach(recheck(_))

    /** Typecheck tree without adapting it, returning a recheck tree.
     *  @param initTree    the unrecheck tree
     *  @param pt          the expected result type
     *  @param locked      the set of type variables of the current typer state that cannot be interpolated
     *                     at the present time
     */
    def recheck(tree: Tree, pt: Type = WildcardType)(using Context): Type = trace(i"rechecking $tree, ${tree.getClass} with $pt", recheckr, show = true) {

      def recheckNamed(tree: NameTree, pt: Type)(using Context): Type =
        val sym = tree.symbol
        tree match
          case tree: Ident => recheckIdent(tree)
          case tree: Select => recheckSelect(tree)
          case tree: Bind => recheckBind(tree, pt)
          case tree: ValDef =>
            if tree.isEmpty then NoType
            else recheckValDef(tree, sym)(using ctx.localContext(tree, sym))
          case tree: DefDef =>
            recheckDefDef(tree, sym)(using ctx.localContext(tree, sym))
          case tree: TypeDef =>
            tree.rhs match
              case impl: Template =>
                recheckClassDef(tree, impl, sym.asClass)(using ctx.localContext(tree, sym))
              case _ =>
                recheckTypeDef(tree, sym)(using ctx.localContext(tree, sym))
          case tree: Labeled => recheckLabeled(tree, pt)

      def recheckUnnamed(tree: Tree, pt: Type): Type = tree match
        case tree: Apply => recheckApply(tree, pt)
        case tree: TypeApply => recheckTypeApply(tree, pt)
        case _: New | _: This | _: Super | _: Literal => tree.tpe
        case tree: Typed => recheckTyped(tree)
        case tree: Assign => recheckAssign(tree)
        case tree: Block => recheckBlock(tree, pt)
        case tree: If => recheckIf(tree, pt)
        case tree: Closure => recheckClosure(tree, pt)
        case tree: Match => recheckMatch(tree, pt)
        case tree: Return => recheckReturn(tree)
        case tree: WhileDo => recheckWhileDo(tree)
        case tree: Try => recheckTry(tree, pt)
        case tree: SeqLiteral => recheckSeqLiteral(tree, pt)
        case tree: Inlined => recheckInlined(tree, pt)
        case tree: TypeTree => recheckTypeTree(tree)
        case tree: Annotated => recheckAnnotated(tree)
        case tree: Alternative => recheckAlternative(tree, pt)
        case tree: PackageDef => recheckPackageDef(tree)
        case tree: Thicket => defn.NothingType

      try
        val result = tree match
          case tree: NameTree => recheckNamed(tree, pt)
          case tree => recheckUnnamed(tree, pt)
        checkConforms(result, pt, tree)
        result
      catch case ex: Exception =>
        println(i"error while rechecking $tree")
        throw ex
    }
    end recheck

    def checkConforms(tpe: Type, pt: Type, tree: Tree)(using Context): Unit = tree match
      case _: DefTree | EmptyTree | _: TypeTree =>
      case _ =>
        val actual = tree.tpe.widenExpr
        val expected = pt.widenExpr
        val isCompatible =
          actual <:< expected
          || expected.isRepeatedParam
             && actual <:< expected.translateFromRepeated(toArray = tree.tpe.isRef(defn.ArrayClass))
        if !isCompatible then
          err.typeMismatch(tree, pt)

    def check()(using Context): Unit =
      val unit = ictx.compilationUnit
      recheck(unit.tpdTree)

  end Rechecker
end Recheck

class TestRecheck extends Recheck:
  def phaseName: String = "recheck"
  //override def isEnabled(using Context) = ctx.settings.YrefineTypes.value
  def newRechecker()(using Context): Rechecker = Rechecker(ctx)


