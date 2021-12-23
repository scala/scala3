package dotty.tools
package dotc
package cc

import core._
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import config.Printers.{capt, recheckr}
import ast.{tpd, untpd, Trees}
import Trees.*
import typer.RefChecks.checkAllOverrides
import util.{SimpleIdentitySet, EqHashMap, SrcPos}
import transform.SymUtils.*
import transform.Recheck
import Recheck.*
import scala.collection.mutable
import CaptureSet.withCaptureSetsExplained

object CheckCaptures:
  import ast.tpd.*

  case class Env(owner: Symbol, captured: CaptureSet, isBoxed: Boolean, outer: Env):
    def isOpen = !captured.isAlwaysEmpty && !isBoxed

  final class SubstParamsMap(from: BindingType, to: List[Type])(using Context)
  extends ApproximatingTypeMap:
    def apply(tp: Type): Type = tp match
      case tp: ParamRef =>
        if tp.binder == from then to(tp.paramNum) else tp
      case tp: NamedType =>
        if tp.prefix `eq` NoPrefix then tp
        else tp.derivedSelect(apply(tp.prefix))
      case _: ThisType =>
        tp
      case _ =>
        mapOver(tp)

  /** Check that a @retains annotation only mentions references that can be tracked
   *  This check is performed at Typer.
   */
  def checkWellformed(ann: Tree)(using Context): Unit =
    for elem <- retainedElems(ann) do
      elem.tpe match
        case ref: CaptureRef =>
          if !ref.canBeTracked then
            report.error(em"$elem cannot be tracked since it is not a parameter or a local variable", elem.srcPos)
        case tpe =>
          report.error(em"$tpe is not a legal type for a capture set", elem.srcPos)

  /** If `tp` is a capturing type, check that all references it mentions have non-empty
   *  capture sets.
   *  This check is performed after capture sets are computed in phase cc.
   */
  def checkWellformedPost(tp: Type, pos: SrcPos)(using Context): Unit = tp match
    case CapturingType(parent, refs, _) =>
      for ref <- refs.elems do
        if ref.captureSetOfInfo.elems.isEmpty then
          report.error(em"$ref cannot be tracked since its capture set is empty", pos)
        else if parent.captureSet.accountsFor(ref) then
          report.warning(em"redundant capture: $parent already accounts for $ref", pos)
    case _ =>

  def checkWellformedPost(ann: Tree)(using Context): Unit =
    /** The lists `elems(i) :: prev.reerse :: elems(0),...,elems(i-1),elems(i+1),elems(n)`
     *  where `n == elems.length-1`, i <- 0..n`.
     */
    def choices(prev: List[Tree], elems: List[Tree]): List[List[Tree]] = elems match
      case Nil => Nil
      case elem :: elems =>
        List(elem :: (prev reverse_::: elems)) ++ choices(elem :: prev, elems)
    for case first :: others <- choices(Nil, retainedElems(ann)) do
      val firstRef = first.toCaptureRef
      val remaining = CaptureSet(others.map(_.toCaptureRef)*)
      if remaining.accountsFor(firstRef) then
        report.warning(em"redundant capture: $remaining already accounts for $firstRef", ann.srcPos)

  private inline val disallowGlobal = true

class CheckCaptures extends Recheck:
  thisPhase =>

  import ast.tpd.*
  import CheckCaptures.*

  def phaseName: String = "cc"
  override def isEnabled(using Context) = ctx.settings.Ycc.value

  def newRechecker()(using Context) = CaptureChecker(ctx)

  override def run(using Context): Unit =
    checkOverrides.traverse(ctx.compilationUnit.tpdTree)
    super.run

  def checkOverrides = new TreeTraverser:
    def traverse(t: Tree)(using Context) =
      t match
        case t: Template =>
          // ^^^ TODO: Can we avoid doing overrides checks twice?
          // We need to do them here since only at this phase CaptureTypes are relevant
          // But maybe we can then elide the check during the RefChecks phase if -Ycc is set?
          checkAllOverrides(ctx.owner.asClass)
        case _ =>
      traverseChildren(t)

  class CaptureChecker(ictx: Context) extends Rechecker(ictx):
    import ast.tpd.*

    private def interpolator(using Context) = new TypeTraverser:
      override def traverse(t: Type) =
        t match
          case CapturingType(parent, refs: CaptureSet.Var, _) =>
            if variance < 0 then capt.println(i"solving $t")
            refs.solve(variance)
            traverse(parent)
          case t @ RefinedType(_, nme.apply, rinfo) if defn.isFunctionOrPolyType(t) =>
            traverse(rinfo)
          case tp: TypeVar =>
          case tp: TypeRef =>
            traverse(tp.prefix)
          case _ =>
            traverseChildren(t)

    private def interpolateVarsIn(tpt: Tree)(using Context): Unit =
      if tpt.isInstanceOf[InferredTypeTree] then
        interpolator.traverse(tpt.knownType)
          .showing(i"solved vars in ${tpt.knownType}", capt)

    private var curEnv: Env = Env(NoSymbol, CaptureSet.empty, false, null)

    private val myCapturedVars: util.EqHashMap[Symbol, CaptureSet] = EqHashMap()
    def capturedVars(sym: Symbol)(using Context) =
      myCapturedVars.getOrElseUpdate(sym,
        if sym.ownersIterator.exists(_.isTerm) then CaptureSet.Var()
        else CaptureSet.empty)

    def markFree(sym: Symbol, pos: SrcPos)(using Context): Unit =
      if sym.exists then
        val ref = sym.termRef
        def recur(env: Env): Unit =
          if env.isOpen && env.owner != sym.enclosure then
            capt.println(i"Mark $sym with cs ${ref.captureSet} free in ${env.owner}")
            checkElem(ref, env.captured, pos)
            if env.owner.isConstructor then
              if env.outer.owner != sym.enclosure then recur(env.outer.outer)
            else recur(env.outer)
        if ref.isTracked then recur(curEnv)

    def includeCallCaptures(sym: Symbol, pos: SrcPos)(using Context): Unit =
      if curEnv.isOpen then
        val ownEnclosure = ctx.owner.enclosingMethodOrClass
        var targetSet = capturedVars(sym)
        if !targetSet.isAlwaysEmpty && sym.enclosure == ownEnclosure then
          targetSet = targetSet.filter {
            case ref: TermRef => ref.symbol.enclosure != ownEnclosure
            case _ => true
          }
        def includeIn(env: Env) =
          capt.println(i"Include call capture $targetSet in ${env.owner}")
          checkSubset(targetSet, env.captured, pos)
        includeIn(curEnv)
        if curEnv.owner.isTerm && curEnv.outer.owner.isClass then
          includeIn(curEnv.outer)

    def includeBoxedCaptures(tp: Type, pos: SrcPos)(using Context): Unit =
      if curEnv.isOpen then
        val ownEnclosure = ctx.owner.enclosingMethodOrClass
        val targetSet = tp.boxedCaptured.filter {
          case ref: TermRef => ref.symbol.enclosure != ownEnclosure
          case _ => true
        }
        checkSubset(targetSet, curEnv.captured, pos)

    def assertSub(cs1: CaptureSet, cs2: CaptureSet)(using Context) =
      assert(cs1.subCaptures(cs2, frozen = false).isOK, i"$cs1 is not a subset of $cs2")

    def checkElem(elem: CaptureRef, cs: CaptureSet, pos: SrcPos)(using Context) =
      val res = elem.singletonCaptureSet.subCaptures(cs, frozen = false)
      if !res.isOK then
        report.error(i"$elem cannot be referenced here; it is not included in allowed capture set ${res.blocking}", pos)

    def checkSubset(cs1: CaptureSet, cs2: CaptureSet, pos: SrcPos)(using Context) =
      val res = cs1.subCaptures(cs2, frozen = false)
      if !res.isOK then
        report.error(i"references $cs1 are not all included in allowed capture set ${res.blocking}", pos)

    override def recheckClosure(tree: Closure, pt: Type)(using Context): Type =
      val cs = capturedVars(tree.meth.symbol)
      recheckr.println(i"typing closure $tree with cvs $cs")
      super.recheckClosure(tree, pt).capturing(cs)
        .showing(i"rechecked $tree, $result", capt)

    override def recheckIdent(tree: Ident)(using Context): Type =
      markFree(tree.symbol, tree.srcPos)
      if tree.symbol.is(Method) then includeCallCaptures(tree.symbol, tree.srcPos)
      super.recheckIdent(tree)

    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Unit =
      try super.recheckValDef(tree, sym)
      finally
        if !sym.is(Param) then
          // parameters with inferred types belong to anonymous methods. We need to wait
          // for more info from the context, so we cannot interpolate. Note that we cannot
          // expect to have all necessary info available at the point where the anonymous
          // function is compiled since we do not propagate expected types into blocks.
          interpolateVarsIn(tree.tpt)

    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Unit =
      val saved = curEnv
      val localSet = capturedVars(sym)
      if !localSet.isAlwaysEmpty then curEnv = Env(sym, localSet, false, curEnv)
      try super.recheckDefDef(tree, sym)
      finally
        interpolateVarsIn(tree.tpt)
        curEnv = saved

    override def recheckClassDef(tree: TypeDef, impl: Template, cls: ClassSymbol)(using Context): Type =
      for param <- cls.paramGetters do
        if param.is(Private) && !param.info.captureSet.isAlwaysEmpty then
          report.error(
            "Implementation restriction: Class parameter with non-empty capture set must be a `val`",
            param.srcPos)
      val saved = curEnv
      val localSet = capturedVars(cls)
      if !localSet.isAlwaysEmpty then curEnv = Env(cls, localSet, false, curEnv)
      try super.recheckClassDef(tree, impl, cls)
      finally curEnv = saved

    /** First half: Refine the type of a constructor call `new C(t_1, ..., t_n)`
     *  to C{val x_1: T_1, ..., x_m: T_m} where x_1, ..., x_m are the tracked
     *  parameters of C and T_1, ..., T_m are the types of the corresponding arguments.
     *
     *  Second half: union of all capture sets of arguments to tracked parameters.
     */
    private def addParamArgRefinements(core: Type, argTypes: List[Type], cls: ClassSymbol)(using Context): (Type, CaptureSet) =
      cls.paramGetters.lazyZip(argTypes).foldLeft((core, CaptureSet.empty: CaptureSet)) { (acc, refine) =>
        val (core, allCaptures) = acc
        val (getter, argType) = refine
        if getter.termRef.isTracked then
          (RefinedType(core, getter.name, argType), allCaptures ++ argType.captureSet)
        else
          (core, allCaptures)
      }

    /** Handle an application of method `sym` with type `mt` to arguments of types `argTypes`.
     *  This means:
     *   - Instantiate result type with actual arguments
     *   - If call is to a constructor:
     *      - remember types of arguments corresponding to tracked
     *        parameters in refinements.
     *      - add capture set of instantiated class to capture set of result type.
     */
    override def instantiate(mt: MethodType, argTypes: List[Type], sym: Symbol)(using Context): Type =
      val ownType =
        if mt.isResultDependent then SubstParamsMap(mt, argTypes)(mt.resType)
        else mt.resType
      if sym.isConstructor then
        val cls = sym.owner.asClass
        val (refined, cs) = addParamArgRefinements(ownType, argTypes, cls)
        refined.capturing(cs ++ capturedVars(cls) ++ capturedVars(sym))
          .showing(i"constr type $mt with $argTypes%, % in $cls = $result", capt)
      else ownType

    def recheckByNameArg(tree: Tree, pt: Type)(using Context): Type =
      val closureDef(mdef) = tree
      val arg = mdef.rhs
      val localSet = CaptureSet.Var()
      curEnv = Env(mdef.symbol, localSet, isBoxed = false, curEnv)
      val result =
        try
          inContext(ctx.withOwner(mdef.symbol)) {
            recheckStart(arg, pt).capturing(localSet)
          }
        finally curEnv = curEnv.outer
      recheckFinish(result, arg, pt)

    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      if tree.symbol == defn.cbnArg then
        recheckByNameArg(tree.args(0), pt)
      else
        includeCallCaptures(tree.symbol, tree.srcPos)
        super.recheckApply(tree, pt)

    override def recheck(tree: Tree, pt: Type = WildcardType)(using Context): Type =
      val res = super.recheck(tree, pt)
      if tree.isTerm then
        includeBoxedCaptures(res, tree.srcPos)
      res

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      Setup(preRecheckPhase, thisPhase, recheckDef)
        .traverse(ctx.compilationUnit.tpdTree)
      withCaptureSetsExplained {
        super.checkUnit(unit)
        PostRefinerCheck.traverse(unit.tpdTree)
        if ctx.settings.YccDebug.value then
          show(unit.tpdTree) // this dows not print tree, but makes its variables visible for dependency printing
      }

    def checkNotGlobal(tree: Tree, tp: Type, allArgs: Tree*)(using Context): Unit =
      for ref <-tp.captureSet.elems do
        val isGlobal = ref match
          case ref: TermRef => ref.isRootCapability
          case _ => false
        if isGlobal then
          val what = if ref.isRootCapability then "universal" else "global"
          val notAllowed = i" is not allowed to capture the $what capability $ref"
          def msg =
            if allArgs.isEmpty then
              i"type of mutable variable ${tree.knownType}$notAllowed"
            else tree match
              case tree: InferredTypeTree =>
                i"""inferred type argument ${tree.knownType}$notAllowed
                    |
                    |The inferred arguments are: [${allArgs.map(_.knownType)}%, %]"""
              case _ => s"type argument$notAllowed"
          report.error(msg, tree.srcPos)

    def checkNotGlobal(tree: Tree, allArgs: Tree*)(using Context): Unit =
      if disallowGlobal then
        tree match
          case LambdaTypeTree(_, restpt) =>
            checkNotGlobal(restpt, allArgs*)
          case _ =>
            checkNotGlobal(tree, tree.knownType, allArgs*)

    def checkNotGlobalDeep(tree: Tree)(using Context): Unit =
      val checker = new TypeTraverser:
        def traverse(tp: Type): Unit = tp match
          case tp: TypeRef =>
            tp.info match
              case TypeBounds(_, hi) => traverse(hi)
              case _ =>
          case tp: TermRef =>
          case _ =>
            checkNotGlobal(tree, tp)
            traverseChildren(tp)
      checker.traverse(tree.knownType)

    object PostRefinerCheck extends TreeTraverser:
      def traverse(tree: Tree)(using Context) =
        tree match
          case _: InferredTypeTree =>
          case tree: TypeTree if !tree.span.isZeroExtent =>
            tree.knownType.foreachPart(
              checkWellformedPost(_, tree.srcPos))
            tree.knownType.foreachPart {
              case AnnotatedType(_, annot) =>
                checkWellformedPost(annot.tree)
              case _ =>
            }
          case tree1 @ TypeApply(fn, args) if disallowGlobal =>
            for arg <- args do
              //println(i"checking $arg in $tree: ${tree.knownType.captureSet}")
              checkNotGlobal(arg, args*)
          case t: ValOrDefDef if t.tpt.isInstanceOf[InferredTypeTree] =>
            val sym = t.symbol
            val isLocal =
              sym.ownersIterator.exists(_.isTerm)
              || sym.accessBoundary(defn.RootClass).isContainedIn(sym.topLevelClass)

            // The following classes of definitions need explicit capture types ...
            if !isLocal                            // ... since external capture types are not inferred
              || sym.owner.is(Trait)               // ... since we do OverridingPairs checking before capture inference
              || sym.allOverriddenSymbols.nonEmpty // ... since we do override checking before capture inference
            then
              val inferred = t.tpt.knownType
              def checkPure(tp: Type) = tp match
                case CapturingType(_, refs, _) if !refs.elems.isEmpty =>
                  val resultStr = if t.isInstanceOf[DefDef] then " result" else ""
                  report.error(
                    em"""Non-local $sym cannot have an inferred$resultStr type
                        |$inferred
                        |with non-empty capture set $refs.
                        |The type needs to be declared explicitly.""", t.srcPos)
                case _ =>
              inferred.foreachPart(checkPure, StopAt.Static)
          case t: ValDef if t.symbol.is(Mutable) =>
            checkNotGlobalDeep(t.tpt)
          case _ =>
        traverseChildren(tree)

    def postRefinerCheck(tree: tpd.Tree)(using Context): Unit =
      PostRefinerCheck.traverse(tree)

  end CaptureChecker
end CheckCaptures
