package dotty.tools
package dotc
package cc

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import config.Printers.{capt, recheckr}
import config.Config
import ast.{tpd, untpd, Trees}
import Trees.*
import typer.RefChecks.{checkAllOverrides, checkParents}
import util.{SimpleIdentitySet, EqHashMap, SrcPos}
import transform.SymUtils.*
import transform.{Recheck, PreRecheck}
import Recheck.*
import scala.collection.mutable
import CaptureSet.{withCaptureSetsExplained, IdempotentCaptRefMap}
import StdNames.nme
import reporting.trace

object CheckCaptures:
  import ast.tpd.*

  class Pre extends PreRecheck, SymTransformer:

    override def isEnabled(using Context) = ctx.settings.Ycc.value

  	/** Reset `private` flags of parameter accessors so that we can refine them
     *  in Setup if they have non-empty capture sets
     */
    def transformSym(sym: SymDenotation)(using Context): SymDenotation =
      if sym.isAllOf(PrivateParamAccessor) && !sym.hasAnnotation(defn.ConstructorOnlyAnnot) then
        sym.copySymDenotation(initFlags = sym.flags &~ Private | Recheck.ResetPrivate)
      else if Synthetics.needsTransform(sym) then
        Synthetics.transformToCC(sym)
      else
        sym
  end Pre

  case class Env(owner: Symbol, captured: CaptureSet, isBoxed: Boolean, outer0: Env | Null):
    def outer = outer0.nn
    def isOpen = !captured.isAlwaysEmpty && !isBoxed

  final class SubstParamsMap(from: BindingType, to: List[Type])(using Context)
  extends ApproximatingTypeMap, IdempotentCaptRefMap:
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
    case CapturingType(parent, refs) =>
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

class CheckCaptures extends Recheck, SymTransformer:
  thisPhase =>

  import ast.tpd.*
  import CheckCaptures.*

  def phaseName: String = "cc"
  override def isEnabled(using Context) = ctx.settings.Ycc.value

  def newRechecker()(using Context) = CaptureChecker(ctx)

  override def run(using Context): Unit =
    checkOverrides.traverse(ctx.compilationUnit.tpdTree)
    super.run

  override def transformSym(sym: SymDenotation)(using Context): SymDenotation =
    if Synthetics.needsTransform(sym) then Synthetics.transformFromCC(sym)
    else super.transformSym(sym)

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

    private def interpolator(startingVariance: Int = 1)(using Context) = new TypeTraverser:
      variance = startingVariance
      override def traverse(t: Type) =
        t match
          case CapturingType(parent, refs: CaptureSet.Var) =>
            if variance < 0 then
              capt.println(i"solving $t")
              refs.solve()
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
        interpolator().traverse(tpt.knownType)
          .showing(i"solved vars in ${tpt.knownType}", capt)

    private var curEnv: Env = Env(NoSymbol, CaptureSet.empty, isBoxed = false, null)

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
      includeBoxedCaptures(tp.boxedCaptured, pos)

    def includeBoxedCaptures(refs: CaptureSet, pos: SrcPos)(using Context): Unit =
      if curEnv.isOpen then
        val ownEnclosure = ctx.owner.enclosingMethodOrClass
        val targetSet = refs.filter {
          case ref: TermRef => ref.symbol.enclosure != ownEnclosure
          case ref: ThisType => true
          case _ => false
        }
        checkSubset(targetSet, curEnv.captured, pos)

    /** If result type of a function type has toplevel boxed captures, propagate
     *  them to the function type as a whole. Such boxed captures
     *  can be created by substitution or as-seen-from. Propagating captures to the
     *  left simulates an unbox operation on the result. I.e. if f has type `A -> box C B`
     *  then in theory we need to unbox with
     *
     *      x => C o- f(x)
     *
     *   and that also propagates C into the type of the unboxing expression.
     *   TODO: Generalize this to boxed captues in other parts of a function type.
     *   Test case in pos-.../boxed1.scala.
     */
    def addResultBoxes(tp: Type)(using Context): Type =
      def includeBoxed(res: Type) =
        //if !res.boxedCaptured.isAlwaysEmpty then
        //  println(i"add boxed $tp from ${res.boxedCaptured}")
        tp.capturing(res.boxedCaptured)
      val tp1 = tp.dealias
      val boxedTp = tp1 match
        case tp1 @ AppliedType(_, args) if defn.isNonRefinedFunction(tp1) =>
          includeBoxed(args.last)
        case tp1 @ RefinedType(_, _, rinfo) if defn.isFunctionType(tp1) =>
          includeBoxed(rinfo.finalResultType)
        case tp1 @ CapturingType(parent, refs) =>
          val boxedParent = addResultBoxes(parent)
          if boxedParent eq parent then tp1
          else boxedParent.capturing(refs)
        case _ =>
          tp1
      if (boxedTp eq tp1) then tp else boxedTp
    end addResultBoxes

    def assertSub(cs1: CaptureSet, cs2: CaptureSet)(using Context) =
      assert(cs1.subCaptures(cs2, frozen = false).isOK, i"$cs1 is not a subset of $cs2")

    def checkElem(elem: CaptureRef, cs: CaptureSet, pos: SrcPos)(using Context) =
      val res = elem.singletonCaptureSet.subCaptures(cs, frozen = false)
      if !res.isOK then
        report.error(i"$elem cannot be referenced here; it is not included in the allowed capture set ${res.blocking}", pos)

    def checkSubset(cs1: CaptureSet, cs2: CaptureSet, pos: SrcPos)(using Context) =
      val res = cs1.subCaptures(cs2, frozen = false)
      if !res.isOK then
        def header =
          if cs1.elems.size == 1 then i"reference ${cs1.elems.toList}%, % is not"
          else i"references $cs1 are not all"
        report.error(i"$header included in allowed capture set ${res.blocking}", pos)

    /** A specialized implementation of the selection rule.
     *
     *  E |- f: Cf { m: Cr R }
     *  ------------------------
     *  E |- f.m: C R
     *
     *  The implementation picks as `C` one of `{f}` or `Cr`, depending on the
     *  outcome of a `mightSubcapture` test. It picks `{f}` if this might subcapture Cr
     *  and Cr otherwise.
     */
    override def recheckSelection(tree: Select, qualType: Type, name: Name)(using Context) = {
      val selType = super.recheckSelection(tree, qualType, name)
      val selCs = selType.widen.captureSet
      if selCs.isAlwaysEmpty
          || selType.widen.isBoxedCapturing
          || qualType.isBoxedCapturing then
        selType
      else
        val qualCs = qualType.captureSet
        capt.println(i"intersect $qualType, ${selType.widen}, $qualCs, $selCs in $tree")
        if qualCs.mightSubcapture(selCs) then
          selType.widen.stripCapturing.capturing(qualCs)
        else
          selType
    }//.showing(i"recheck sel $tree, $qualType = $result")

    override def recheckClosure(tree: Closure, pt: Type)(using Context): Type =
      val cs = capturedVars(tree.meth.symbol)
      capt.println(i"typing closure $tree with cvs $cs")
      super.recheckClosure(tree, pt).capturing(cs)
        .showing(i"rechecked $tree / $pt = $result", capt)

    override def recheckBlock(block: Block, pt: Type)(using Context): Type =
      block match
        case closureDef(mdef) =>
          pt.dealias match
            case defn.FunctionOf(ptformals, _, _, _) if ptformals.forall(_.captureSet.isAlwaysEmpty) =>
              // Redo setup of the anonymous function so that formal parameters don't
              // get capture sets. This is important to avoid false widenings to `*`
              // when taking the base type of the actual clsoures's dependent function
              // type so that it conforms to the expected non-dependent function type.
              // See withLogFile.scala for a test case.
              val meth = mdef.symbol
              // First, undo the previous setup which installed a completer for `meth`.
              atPhase(preRecheckPhase.prev)(meth.denot.copySymDenotation())
                .installAfter(preRecheckPhase)
              // Next, update all parameter symbols to match expected formals
              meth.paramSymss.head.lazyZip(ptformals).foreach { (psym, pformal) =>
                psym.copySymDenotation(info = pformal).installAfter(preRecheckPhase)
              }
              // Next, update types of parameter ValDefs
              mdef.paramss.head.lazyZip(ptformals).foreach { (param, pformal) =>
                val ValDef(_, tpt, _) = param: @unchecked
                tpt.rememberTypeAlways(pformal)
              }
              // Next, install a new completer reflecting the new parameters for the anonymous method
              val completer = new LazyType:
                def complete(denot: SymDenotation)(using Context) =
                  denot.info = MethodType(ptformals, mdef.tpt.knownType)
                    .showing(i"simplify info of $meth to $result", capt)
                  recheckDef(mdef, meth)
              meth.copySymDenotation(info = completer, initFlags = meth.flags &~ Touched)
                .installAfter(preRecheckPhase)
            case _ =>
        case _ =>
      super.recheckBlock(block, pt)

    override def recheckIdent(tree: Ident)(using Context): Type =
      markFree(tree.symbol, tree.srcPos)
      if tree.symbol.is(Method) then includeCallCaptures(tree.symbol, tree.srcPos)
      super.recheckIdent(tree)

    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Unit =
      try
        if !sym.is(Module) then super.recheckValDef(tree, sym)
      finally
        if !sym.is(Param) then
          // parameters with inferred types belong to anonymous methods. We need to wait
          // for more info from the context, so we cannot interpolate. Note that we cannot
          // expect to have all necessary info available at the point where the anonymous
          // function is compiled since we do not propagate expected types into blocks.
          interpolateVarsIn(tree.tpt)

    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Unit =
      if !Synthetics.isExcluded(sym) then
        val saved = curEnv
        val localSet = capturedVars(sym)
        if !localSet.isAlwaysEmpty then curEnv = Env(sym, localSet, isBoxed = false, curEnv)
        try super.recheckDefDef(tree, sym)
        finally
          interpolateVarsIn(tree.tpt)
          curEnv = saved

    override def recheckClassDef(tree: TypeDef, impl: Template, cls: ClassSymbol)(using Context): Type =
      val saved = curEnv
      val localSet = capturedVars(cls)
      for parent <- impl.parents do
        checkSubset(capturedVars(parent.tpe.classSymbol), localSet, parent.srcPos)
      if !localSet.isAlwaysEmpty then curEnv = Env(cls, localSet, isBoxed = false, curEnv)
      try
        val thisSet = cls.classInfo.selfType.captureSet.withDescription(i"of the self type of $cls")
        checkSubset(localSet, thisSet, tree.srcPos)
        for param <- cls.paramGetters do
          checkSubset(param.termRef.captureSet, thisSet, param.srcPos)
        super.recheckClassDef(tree, impl, cls)
      finally
        curEnv = saved

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

        /** First half: Refine the type of a constructor call `new C(t_1, ..., t_n)`
         *  to C{val x_1: T_1, ..., x_m: T_m} where x_1, ..., x_m are the tracked
         *  parameters of C and T_1, ..., T_m are the types of the corresponding arguments.
         *
         *  Second half: union of all capture sets of arguments to tracked parameters.
         */
        def addParamArgRefinements(core: Type, initCs: CaptureSet): (Type, CaptureSet) =
          mt.paramNames.lazyZip(argTypes).foldLeft((core, initCs)) { (acc, refine) =>
            val (core, allCaptures) = acc
            val (getterName, argType) = refine
            val getter = cls.info.member(getterName).suchThat(_.is(ParamAccessor)).symbol
            if getter.termRef.isTracked && !getter.is(Private)
            then (RefinedType(core, getterName, argType), allCaptures ++ argType.captureSet)
            else (core, allCaptures)
          }

        def augmentConstructorType(core: Type, initCs: CaptureSet): Type = core match
          case core: MethodType =>
            core.derivedLambdaType(resType = augmentConstructorType(core.resType, initCs))
          case CapturingType(parent, refs) =>
            augmentConstructorType(parent, initCs ++ refs)
          case _ =>
            val (refined, cs) = addParamArgRefinements(core, initCs)
            refined.capturing(cs)

        val augmented = augmentConstructorType(ownType, CaptureSet.empty)
        { if augmented.isInstanceOf[MethodType] then augmented
          else augmented.capturing(capturedVars(cls) ++ capturedVars(sym))
        }.showing(i"constr type $mt with $argTypes%, % in $cls = $result", capt)
      else ownType
    end instantiate

    def recheckByNameArg(tree: Tree, pt: Type)(using Context): Type =
      val closureDef(mdef) = tree: @unchecked
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

    /** A specialized implementation of the apply rule.
     *
     *  E |- f: Cf (Ra -> Cr Rr)
     *  E |- a: Ra
     *  ------------------------
     *  E |- f a: C Rr
     *
     *  The implementation picks as `C` one of `{f, a}` or `Cr`, depending on the
     *  outcome of a `mightSubcapture` test. It picks `{f, a}` if this might subcapture Cr
     *  and Cr otherwise.
     */
    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      includeCallCaptures(tree.symbol, tree.srcPos)
      super.recheckApply(tree, pt) match
        case tp @ CapturingType(tp1, refs) =>
          tree.fun match
            case Select(qual, _)
            if !tree.fun.symbol.isConstructor
                && qual.tpe.captureSet.mightSubcapture(refs)
                && tree.args.forall(_.tpe.captureSet.mightSubcapture(refs))
                && !qual.tpe.isBoxedCapturing
                && !tree.args.exists(_.tpe.isBoxedCapturing)
            =>
              tp.derivedCapturingType(tp1, tree.args.foldLeft(qual.tpe.captureSet)((cs, arg) =>
                cs ++ arg.tpe.captureSet))
                .showing(i"narrow $tree: $tp, refs = $refs, qual = ${qual.tpe.captureSet} --> $result", capt)
            case _ => tp
        case tp => tp

    override def recheckTyped(tree: Typed)(using Context): Type =
      tree.tpt.tpe match
        case AnnotatedType(_, annot) if annot.symbol == defn.RequiresCapabilityAnnot =>
          annot.tree match
            case Apply(_, cap :: Nil) =>
              markFree(cap.symbol, tree.srcPos)
            case _ =>
        case _ =>
      super.recheckTyped(tree)

    override def recheck(tree: Tree, pt: Type = WildcardType)(using Context): Type =
      val res =
        if tree.isTerm && pt.isBoxedCapturing then
          val saved = curEnv
          curEnv = Env(curEnv.owner, CaptureSet.Var(), isBoxed = true, curEnv)
          try super.recheck(tree, pt)
          finally curEnv = saved
        else super.recheck(tree, pt)
      if tree.isTerm then
        includeBoxedCaptures(res, tree.srcPos)
      res

    override def recheckFinish(tpe: Type, tree: Tree, pt: Type)(using Context): Type =
      val typeToCheck = tree match
        case _: Ident | _: Select | _: Apply | _: TypeApply if tree.symbol.unboxesResult =>
          tpe
        case _: Try =>
          tpe
        case ValDef(_, tpt, _) if tree.symbol.is(Mutable) =>
          tree.symbol.info
        case _ =>
          NoType
      def checkNotUniversal(tp: Type): Unit = tp.widenDealias match
        case wtp @ CapturingType(parent, refs) =>
          refs.disallowRootCapability { () =>
            val kind = if tree.isInstanceOf[ValDef] then "mutable variable" else "expression"
            report.error(
              em"""The $kind's type $wtp is not allowed to capture the root capability `*`.
                  |This usually means that a capability persists longer than its allowed lifetime.""",
              tree.srcPos)
          }
          checkNotUniversal(parent)
        case _ =>
      checkNotUniversal(typeToCheck)
      val tpe1 = if false && tree.isTerm then addResultBoxes(tpe) else tpe
      super.recheckFinish(tpe1, tree, pt)

    /** This method implements the rule outlined in #14390:
     *  When checking an expression `e: T` against an expected type `Cx Tx`
     *  where the capture set of `Cx` contains this and any method inside the class
     *  `Cls` of `this` that contains `e` has only pure parameters, add to `Cx`
     *  all references to variables or this-references in that capture set of `T`
     *  that are outside `Cls`. These are all accessed through this, so we can assume
     *  they are already accounted for by `Cx` and adding them explicitly to `Cx`
     *  changes nothing.
     */
    override def checkConformsExpr(actual: Type, expected: Type, tree: Tree)(using Context): Unit =
      def isPure(info: Type): Boolean = info match
        case info: PolyType => isPure(info.resType)
        case info: MethodType => info.paramInfos.forall(_.captureSet.isAlwaysEmpty) && isPure(info.resType)
        case _ => true
      def isPureContext(owner: Symbol, limit: Symbol): Boolean =
        if owner == limit then true
        else if !owner.exists then false
        else isPure(owner.info) && isPureContext(owner.owner, limit)
      def augment(erefs: CaptureSet, arefs: CaptureSet): CaptureSet =
        (erefs /: erefs.elems) { (erefs, eref) =>
          eref match
            case eref: ThisType if isPureContext(ctx.owner, eref.cls) =>
              erefs ++ arefs.filter {
                case aref: TermRef => eref.cls.isContainedIn(aref.symbol.owner)
                case aref: ThisType => eref.cls.isContainedIn(aref.cls)
                case _ => false
              }
            case _ =>
              erefs
        }
      val expected1 = expected match
        case CapturingType(ecore, erefs) =>
          val erefs1 = augment(erefs, actual.captureSet)
          if erefs1 ne erefs then
            capt.println(i"augmented $expected from ${actual.captureSet} --> $erefs1")
          expected.derivedCapturingType(ecore, erefs1)
        case _ =>
          expected
      val normActual = adaptBoxed(actual, expected1, tree.srcPos)
      //println(i"check conforms $normActual <<< $expected1")
      super.checkConformsExpr(normActual, expected1, tree)

    /** Adapt `actual` type to `expected` type by inserting boxing and unboxing conversions */
    def adaptBoxed(actual: Type, expected: Type, pos: SrcPos)(using Context): Type =

      def adaptFun(actual: Type, aargs: List[Type], ares: Type, expected: Type,
          covariant: Boolean,
          reconstruct: (List[Type], Type) => Type): Type =
        val (eargs, eres) = expected.dealias match
          case defn.FunctionOf(eargs, eres, _, _) => (eargs, eres)
          case _ => (aargs.map(_ => WildcardType), WildcardType)
        val aargs1 = aargs.zipWithConserve(eargs)(adapt(_, _, !covariant))
        val ares1 = adapt(ares, eres, covariant)
        if (ares1 eq ares) && (aargs1 eq aargs) then actual
        else reconstruct(aargs1, ares1)

      def adapt(actual: Type, expected: Type, covariant: Boolean): Type = actual.dealias match
        case actual @ CapturingType(parent, refs) =>
          val parent1 = adapt(parent, expected, covariant)
          if actual.isBoxed != expected.isBoxedCapturing then
            val uni = if covariant then refs.isUniversal else expected.captureSet.isUniversal
            if uni then // TODO: better to constrain the set to be not universal
              capt.println(i"ABORTING $actual vs $expected")
              actual
            else
              if covariant == actual.isBoxed then includeBoxedCaptures(refs, pos)
              CapturingType(parent1, refs, boxed = !actual.isBoxed)
          else if parent1 eq parent then actual
          else CapturingType(parent1, refs, boxed = actual.isBoxed)
        case actual @ AppliedType(tycon, args) if defn.isNonRefinedFunction(actual) =>
          adaptFun(actual, args.init, args.last, expected, covariant,
              (aargs1, ares1) => actual.derivedAppliedType(tycon, aargs1 :+ ares1))
        case actual @ RefinedType(_, _, rinfo: MethodType) if defn.isFunctionType(actual) =>
          adaptFun(actual, rinfo.paramInfos, rinfo.resType, expected, covariant,
            (aargs1, ares1) =>
              rinfo.derivedLambdaType(paramInfos = aargs1, resType = ares1)
                .toFunctionType(isJava = false, alwaysDependent = true))
        case _ => actual

      if Config.checkBoxes then
        var actualw = actual.widenDealias
        actual match
          case ref: CaptureRef if ref.isTracked =>
            actualw match
              case CapturingType(p, refs) =>
                actualw = actualw.derivedCapturingType(p, ref.singletonCaptureSet)
              case _ =>
          case _ =>
        val adapted = adapt(actualw, expected, covariant = true)
        if adapted ne actualw then
          capt.println(i"adapt boxed $actual vs $expected ===> $adapted")
          adapted
        else actual
      else actual
    end adaptBoxed

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      Setup(preRecheckPhase, thisPhase, recheckDef)
        .traverse(ctx.compilationUnit.tpdTree)
      withCaptureSetsExplained {
        super.checkUnit(unit)
        checkSelfTypes(unit.tpdTree)
        postCheck(unit.tpdTree)
        if ctx.settings.YccDebug.value then
          show(unit.tpdTree) // this does not print tree, but makes its variables visible for dependency printing
      }

    /** Check that self types of subclasses conform to self types of super classes.
     *  (See comment below how this is achieved). The check assumes that classes
     *  without an explicit self type have the universal capture set `{*}` on the
     *  self type. If a class without explicit self type is not `effectivelyFinal`
     *  it is checked that the inferred self type is universal, in order to assure
     *  that joint and separate compilation give the same result.
     */
    def checkSelfTypes(unit: tpd.Tree)(using Context): Unit =
      val parentTrees = mutable.HashMap[Symbol, List[Tree]]()
      unit.foreachSubTree {
        case cdef @ TypeDef(_, impl: Template) => parentTrees(cdef.symbol) = impl.parents
        case _ =>
      }
      // Perform self type checking. The problem here is that `checkParents` compares a
      // self type of a subclass with the result of an asSeenFrom of the self type of the
      // superclass. That's no good. We need to constrain the original superclass self type
      // capture set, not the set mapped by asSeenFrom.
      //
      // Instead, we proceed from parent classes to child classes. For every class
      // we first check its parents, and then interpolate the self type to an
      // upper approximation that satisfies all constraints on its capture set.
      // That means all capture sets of parent self types are constants, so mapping
      // them with asSeenFrom is OK.
      while parentTrees.nonEmpty do
        val roots = parentTrees.keysIterator.filter {
          cls => !parentTrees(cls).exists(ptree => parentTrees.contains(ptree.tpe.classSymbol))
        }
        assert(roots.nonEmpty)
        for root <- roots do
          checkParents(root, parentTrees(root))
          val selfType = root.asClass.classInfo.selfType
          interpolator(startingVariance = -1).traverse(selfType)
          if !root.isEffectivelySealed  then
            selfType match
              case CapturingType(_, refs: CaptureSet.Var) if !refs.isUniversal =>
                report.error(
                  i"""$root needs an explicitly declared self type since its
                     |inferred self type $selfType
                     |is not visible in other compilation units that define subclasses.""",
                  root.srcPos)
              case _ =>
          parentTrees -= root
          capt.println(i"checked $root with $selfType")
    end checkSelfTypes

    /** Perform the following kinds of checks
     *   - Check all explicitly written capturing types for well-formedness using `checkWellFormedPost`.
     *   - Check that externally visible `val`s or `def`s have empty capture sets. If not
     *     suggest an explicit type. This is so that separate compilation (where external
     *     symbols have empty capture sets) gives the same results as jount compilation.
     */
    def postCheck(unit: tpd.Tree)(using Context): Unit =
      unit.foreachSubTree {
        case _: InferredTypeTree =>
        case tree: TypeTree if !tree.span.isZeroExtent =>
          tree.knownType.foreachPart(
            checkWellformedPost(_, tree.srcPos))
          tree.knownType.foreachPart {
            case AnnotatedType(_, annot) =>
              checkWellformedPost(annot.tree)
            case _ =>
          }
        case t: ValOrDefDef
        if t.tpt.isInstanceOf[InferredTypeTree] && !Synthetics.isExcluded(t.symbol) =>
          val sym = t.symbol
          val isLocal =
            sym.owner.ownersIterator.exists(_.isTerm)
            || sym.accessBoundary(defn.RootClass).isContainedIn(sym.topLevelClass)

          // The following classes of definitions need explicit capture types ...
          if !isLocal                            // ... since external capture types are not inferred
            || sym.owner.is(Trait)               // ... since we do OverridingPairs checking before capture inference
            || sym.allOverriddenSymbols.nonEmpty // ... since we do override checking before capture inference
          then
            val inferred = t.tpt.knownType
            def checkPure(tp: Type) = tp match
              case CapturingType(_, refs) if !refs.elems.isEmpty =>
                val resultStr = if t.isInstanceOf[DefDef] then " result" else ""
                report.error(
                  em"""Non-local $sym cannot have an inferred$resultStr type
                      |$inferred
                      |with non-empty capture set $refs.
                      |The type needs to be declared explicitly.""", t.srcPos)
              case _ =>
            inferred.foreachPart(checkPure, StopAt.Static)
        case _ =>
      }

  end CaptureChecker
end CheckCaptures
