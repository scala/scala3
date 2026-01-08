package dotty.tools
package dotc
package cc

import core.*
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import Annotations.Annotation
import config.Feature
import config.Printers.{capt, captDebug}
import ast.tpd, tpd.*
import transform.{PreRecheck, Recheck}, Recheck.*
import Synthetics.isExcluded
import util.SimpleIdentitySet
import util.chaining.*
import reporting.Message
import printing.{Printer, Texts}, Texts.{Text, Str}
import collection.mutable
import CCState.*
import CheckCaptures.CheckerAPI
import NamerOps.methodType
import NameKinds.{CanThrowEvidenceName, TryOwnerName}
import Capabilities.*

/** Operations accessed from CheckCaptures */
trait SetupAPI:

  /** Setup procedure to run for each compilation unit
   *   @param tree       the typed tree of the unit to check
   *   @param checker    the capture checker which will run subsequently.
   */
  def setupUnit(tree: Tree, checker: CheckerAPI)(using Context): Unit

  /** Symbol is a term member of a class that was not capture checked
   *  The info of these symbols is made fluid.
   */
  def isPreCC(sym: Symbol)(using Context): Boolean

  /** Check to do after the capture checking traversal */
  def postCheck()(using Context): Unit

  /** A map from currently compiled class symbols to those of their fields
   *  that have an explicit type given. Used in `captureSetImpliedByFields`
   *  to avoid forcing fields with inferred types prematurely. The test file
   *  where this matters is i24335.scala. The precise failure scenario which
   *  this avoids is described in #24335.
   */
  def fieldsWithExplicitTypes: collection.Map[ClassSymbol, List[Symbol]]

  /** Used for error reporting:
   *  Maps mutable variables to the symbols that capture them (in the
   *  CheckCaptures sense, i.e. symbol is referred to from a different method
   *  than the one it is defined in).
   */
  def capturedBy: collection.Map[Symbol, Symbol]

  /** Used for error reporting:
   *  Maps anonymous functions appearing as function arguments to
   *  the function that is called.
   */
  def anonFunCallee: collection.Map[Symbol, Symbol]

end SetupAPI

object Setup:

  val name: String = "setupCC"
  val description: String = "prepare compilation unit for capture checking"

  /** Recognizer for `res $throws exc`, returning `(res, exc)` in case of success */
  object throwsAlias:
    def unapply(tp: Type)(using Context): Option[(Type, Type)] = tp match
      case AppliedType(tycon, res :: exc :: Nil) if tycon.typeSymbol == defn.throwsAlias =>
        Some((res, exc))
      case _ =>
        None

  def firstCanThrowEvidence(body: Tree)(using Context): Option[Tree] = body match
    case Block(stats, expr) =>
      if stats.isEmpty then firstCanThrowEvidence(expr)
      else stats.find:
        case vd: ValDef => vd.symbol.name.is(CanThrowEvidenceName)
        case _ => false
    case _ => None

end Setup
import Setup.*

/** Phase that sets up everthing for capture checking.
 *
 *  A tree traverser that prepares a compilation unit to be capture checked.
 *  It does the following:
 *    - For every inferred type, drop any retains annotations,
 *      add capture sets to all its parts, add refinements to class types and function types.
 *      (c.f. mapInferred)
 *    - For explicit capturing types, expand throws aliases to the underlying (pure) function,
 *      and add some implied capture sets to curried functions (c.f. expandThrowsAlias, expandAbbreviations).
 *    - Add capture sets to self types of classes and objects, unless the self type was written explicitly.
 *    - Box the types of mutable variables and type arguments to methods (type arguments of types
 *      are boxed on access).
 *    - Link the external types of val and def symbols with the inferred types based on their parameter symbols.
 */
class Setup extends PreRecheck, SymTransformer, SetupAPI:
  thisPhase =>

  override def phaseName: String = Setup.name

  override def description: String = Setup.description

  override def isRunnable(using Context) =
    super.isRunnable && Feature.ccEnabledSomewhere

  /** A set containing symbols whose denotation is in train of being updated
   *  Used to suppress transforming the denotations of these symbols.
   */
  private val toBeUpdated = new mutable.HashSet[Symbol]

  /** Drops `private` from the flags of `symd` provided it is
   *  a parameter accessor that's not `constructorOnly` or `uncheckedCaptured`
   *  and that contains at least one @retains in co- or in-variant position.
   *  The @retains might be implicit for a type deriving from `Capability`.
   */
  private def newFlagsFor(symd: SymDenotation)(using Context): FlagSet =

    object containsCovarRetains extends TypeAccumulator[Boolean]:
      val seen = util.HashSet[Symbol]()
      def apply(x: Boolean, tp: Type): Boolean =
        if x then true
        else if tp.derivesFromCapability && variance >= 0 then true
        else tp.dealiasKeepAnnots match
          case AnnotatedType(_, ann) if ann.symbol.isRetains && variance >= 0 => true
          case t: TypeRef if t.symbol.isAbstractOrParamType && !seen.contains(t.symbol) =>
            seen += t.symbol
            apply(x, t.info.bounds.hi)
          case tp1 =>
            foldOver(x, tp1)
      def apply(tp: Type): Boolean = apply(false, tp)

    if symd.symbol.isRefiningParamAccessor
        && symd.is(Private)
        && symd.owner.is(CaptureChecked)
        && containsCovarRetains(symd.symbol.originDenotation.info)
    then symd.flags &~ Private
    else symd.flags
  end newFlagsFor

  /** Symbol is a term member of a class that was not capture checked
   *  The info of these symbols is made fluid.
   */
  def isPreCC(sym: Symbol)(using Context): Boolean =
    sym.isTerm && sym.maybeOwner.isClass
    && !sym.is(Module)
    && !sym.owner.is(CaptureChecked)
    && !defn.isFunctionSymbol(sym.owner)

  /** The symbol transformer of this phase.
   *   - Resets `private` flags of parameter accessors so that we can refine them
   *     in Setup if they have non-empty capture sets.
   *   - Special handling of some symbols defined for case classes.
   *  Enabled only until recheck is finished, and provided some compilation unit
   *  is CC-enabled.
   */
  def transformSym(symd: SymDenotation)(using Context): SymDenotation =
    if !pastRecheck && Feature.ccEnabledSomewhere then
      val sym = symd.symbol
      def mappedInfo =
        if toBeUpdated.contains(sym) then
          symd.info // don't transform symbols that will anyway be updated
        else if sym.isArrayUnderStrictMut then
          val cinfo: ClassInfo = sym.info.asInstanceOf
          cinfo.derivedClassInfo(
            declaredParents = cinfo.declaredParents :+ defn.Caps_Mutable.typeRef)
        else
          transformExplicitType(symd.info, sym)
      if Synthetics.needsTransform(symd) then
        Synthetics.transform(symd, mappedInfo)
      else if isPreCC(sym) then
        symd.copySymDenotation(info = fluidify(sym.info))
      else if symd.owner.isTerm || symd.is(CaptureChecked) || symd.owner.is(CaptureChecked) then
        val newFlags = newFlagsFor(symd)
        val newInfo = mappedInfo
        if sym.isClass then
          sym.thisType.asInstanceOf[ThisType].invalidateCaches()
        if newFlags != symd.flags || (newInfo ne sym.info)
        then symd.copySymDenotation(initFlags = newFlags, info = newInfo)
        else symd
      else symd
    else symd
  end transformSym

  private trait SetupTypeMap extends FollowAliasesMap:
    private var isTopLevel = true

    protected def innerApply(tp: Type): Type

    final def apply(tp: Type) =
      val saved = isTopLevel
      if variance < 0 then isTopLevel = false
      try tp match
        case defn.RefinedFunctionOf(rinfo: MethodType) =>
          val rinfo1 = apply(rinfo)
          if rinfo1 ne rinfo then rinfo1.toFunctionType(alwaysDependent = true)
          else tp
        case _ =>
          innerApply(tp)
      finally isTopLevel = saved

    override def mapArg(arg: Type, tparam: ParamInfo): Type =
      super.mapArg(Recheck.mapExprType(arg), tparam)

    /** Map parametric functions with results that have a capture set somewhere
     *  to dependent functions.
     */
    protected def normalizeFunctions(tp: Type, original: Type, expandAlways: Boolean = false)(using Context): Type =
      tp match
      case AppliedType(tycon, args)
      if defn.isNonRefinedFunction(tp) && isTopLevel =>
        // Expand if we have an applied type that underwent some addition of capture sets
        val expand = expandAlways || original.match
          case AppliedType(`tycon`, args0) => args0.last ne args.last
          case _ => false
        if expand then
          val (fn: RefinedType) = depFun(
            args.init, args.last,
            isContextual = defn.isContextFunctionClass(tycon.classSymbol))
              .showing(i"add function refinement $tp ($tycon, ${args.init}, ${args.last}) --> $result", capt)
            .runtimeChecked
          RefinedType.inferred(fn.parent, fn.refinedName, fn.refinedInfo)
        else tp
      case _ => tp

    /** Pull out an embedded capture set from a part of `tp` */
    def normalizeCaptures(tp: Type)(using Context): Type = tp match
      case tp @ RefinedType(parent @ CapturingType(parent1, refs), rname, rinfo) =>
        CapturingType(tp.derivedRefinedType(parent1, rname, rinfo), refs, parent.isBoxed)
      case tp: RecType =>
        tp.parent match
          case parent @ CapturingType(parent1, refs) =>
            CapturingType(tp.derivedRecType(parent1), refs, parent.isBoxed)
          case _ =>
            tp // can return `tp` here since unlike RefinedTypes, RecTypes are never created
                // by `mapInferred`. Hence if the underlying type admits capture variables
                // a variable was already added, and the first case above would apply.
      case AndType(tp1 @ CapturingType(parent1, refs1), tp2 @ CapturingType(parent2, refs2)) =>
        assert(tp1.isBoxed == tp2.isBoxed)
        CapturingType(AndType(parent1, parent2), refs1 ** refs2, tp1.isBoxed)
      case tp @ OrType(tp1 @ CapturingType(parent1, refs1), tp2 @ CapturingType(parent2, refs2)) =>
        assert(tp1.isBoxed == tp2.isBoxed)
        CapturingType(OrType(parent1, parent2, tp.isSoft), refs1 ++ refs2, tp1.isBoxed)
      case tp @ OrType(tp1 @ CapturingType(parent1, refs1), tp2) =>
        CapturingType(OrType(parent1, tp2, tp.isSoft), refs1, tp1.isBoxed)
      case tp @ OrType(tp1, tp2 @ CapturingType(parent2, refs2)) =>
        CapturingType(OrType(tp1, parent2, tp.isSoft), refs2, tp2.isBoxed)
      case tp @ AppliedType(tycon, args)
      if !defn.isFunctionClass(tp.dealias.typeSymbol) && (tp.dealias eq tp) =>
        tp.derivedAppliedType(tycon, args.mapConserve(_.boxDeeply))
      case tp: RealTypeBounds =>
        tp.derivedTypeBounds(tp.lo, tp.hi.boxDeeply)
      case tp: LazyRef =>
        normalizeCaptures(tp.ref)
      case _ =>
        tp

  end SetupTypeMap

  /** Transform the type of an InferredTypeTree by performing the following transformation
    * steps everywhere in the type:
    *  1. Drop retains annotations
    *  2. Turn plain function types into dependent function types, so that
    *     we can refer to their parameters in capture sets. Currently this is
    *     only done at the toplevel, i.e. for function types that are not
    *     themselves argument types of other function types. Without this restriction
    *     pos.../lists.scala and pos/...curried-shorthands.scala fail.
    *     Need to figure out why.
    *  3. Refine other class types C by adding capture set variables to their parameter getters
    *     (see addCaptureRefinements), provided `refine` is true.
    *  4. Add capture set variables to all types that can be tracked
    *  5. Perform normalizeCaptures
    *
    *  Polytype bounds are only cleaned using step 1, but not otherwise transformed.
    */
  private def transformInferredType(tp: Type)(using Context): Type =
    def mapInferred(inCaptureRefinement: Boolean): TypeMap = new TypeMap with SetupTypeMap:
      override def toString = "map inferred"

      var refiningNames: Set[Name] = Set()

      /** Refine a possibly applied class type C where the class has tracked parameters
       *  x_1: T_1, ..., x_n: T_n to C { val x_1: T_1^{CV_1}, ..., val x_n: T_n^{CV_n} }
       *  where CV_1, ..., CV_n are fresh capture set variables.
       */
      def addCaptureRefinements(tp: Type): Type = tp match
        case _: TypeRef | _: AppliedType if !inCaptureRefinement && tp.typeParams.isEmpty =>
          tp.typeSymbol match
            case cls: ClassSymbol
            if !defn.isFunctionClass(cls) && cls.is(CaptureChecked) =>
              cls.paramGetters.foldLeft(tp): (core, getter) =>
                if atPhase(thisPhase.next)(getter.hasTrackedParts)
                    && getter.isRefiningParamAccessor
                    && !refiningNames.contains(getter.name) // Don't add a refinement if we have already an explicit one for the same name
                then
                  val getterType =
                    mapInferred(inCaptureRefinement = true)(tp.memberInfo(getter)).strippedDealias
                  RefinedType.precise(core, getter.name,
                      CapturingType(getterType,
                        CaptureSet.ProperVar(ctx.owner, isRefining = true)))
                    .showing(i"add capture refinement $tp --> $result", capt)
                else
                  core
            case _ => tp
        case _ => tp

      def innerApply(tp: Type) =
        val tp1 = tp match
          case AnnotatedType(parent, annot) if annot.symbol.isRetains =>
            // Drop explicit retains annotations
            apply(parent)
          case tp: TypeLambda =>
            // Don't recurse into parameter bounds, just cleanup any stray retains annotations
            ccState.withoutMappedFutureElems:
              tp.derivedLambdaType(
                paramInfos = tp.paramInfos.mapConserve(_.dropAllRetains.bounds),
                resType = this(tp.resType))
          case tp @ RefinedType(parent, rname, rinfo) =>
            val saved = refiningNames
            refiningNames += rname
            val parent1 = try this(parent) finally refiningNames = saved
            tp.derivedRefinedType(parent1, rname, this(rinfo))
          case _ =>
            mapFollowingAliases(tp)
        addVar(
          addCaptureRefinements(normalizeCaptures(normalizeFunctions(tp1, tp))),
          ctx.owner,
          isRefining = inCaptureRefinement)
    end mapInferred

    try
      val tp1 = mapInferred(inCaptureRefinement = false)(tp)
      val tp2 = toResultInResults(NoSymbol, _ => assert(false))(tp1)
      if tp2 ne tp then capt.println(i"expanded inferred in ${ctx.owner}: $tp  -->  $tp1  -->  $tp2")
      tp2
    catch case ex: AssertionError =>
      println(i"error while mapping inferred $tp")
      throw ex
  end transformInferredType

  /** Transform an explicitly given type by performing the following transformation
   *  steps everywhere in the type:
   *   1. Expand throws aliases to contextual function type with CanThrow parameters
   *   2. Map types with retains annotations to CapturingTypes
   *   3. Add universal capture sets to types deriving from Capability
   *   4. Map `cap` in function result types to existentially bound variables.
   *   5. Schedule deferred well-formed tests for types with retains annotations.
   *   6. Perform normalizeCaptures
   */
  private def transformExplicitType(tp: Type, sym: Symbol, tptToCheck: Tree = EmptyTree)(using Context): Type =

    def fail(msg: Message) =
      if !tptToCheck.isEmpty then report.error(msg, tptToCheck.srcPos)

    /** If C derives from Capability and we have a C^cs in source, we leave it as is
     *  instead of expanding it to C^{cap}^cs. We do this by stripping capability-generated
     *  universal capture sets from the parent of a CapturingType.
     */
    def stripImpliedCaptureSet(tp: Type): Type = tp match
      case tp @ CapturingType(parent, refs)
      if refs.isInstanceOf[CaptureSet.CSImpliedByCapability] && !tp.isBoxedCapturing =>
        parent
      case tp: AliasingBounds =>
        tp.derivedAlias(stripImpliedCaptureSet(tp.alias))
      case tp: RealTypeBounds =>
        tp.derivedTypeBounds(stripImpliedCaptureSet(tp.lo), stripImpliedCaptureSet(tp.hi))
      case _ => tp

    object toCapturing extends DeepTypeMap, SetupTypeMap:
      override def toString = "transformExplicitType"

      var keepFunAliases = true
      var keptFunAliases = false

      /** Expand $throws aliases. This is hard-coded here since $throws aliases in stdlib
        * are defined with `?=>` rather than `?->`.
        * We also have to add a capture set to the last expanded throws alias. I.e.
        *       T $throws E1 $throws E2
        * expands to
        *       (erased x$0: CanThrow[E1]) ?-> (erased x$1: CanThrow[E1]) ?->{x$0} T
        */
      private def expandThrowsAlias(res: Type, exc: Type, encl: List[MethodType]): Type =
        val paramType = AnnotatedType(
            defn.CanThrowClass.typeRef.appliedTo(exc),
            Annotation(defn.ErasedParamAnnot, defn.CanThrowClass.span))
        val resDecomposed = throwsAlias.unapply(res)
        val paramName = nme.syntheticParamName(encl.length)
        val mt = ContextualMethodType(paramName :: Nil)(
            _ => paramType :: Nil,
            mt => resDecomposed match
              case Some((res1, exc1)) => expandThrowsAlias(res1, exc1, mt :: encl)
              case _ => res
          )
        val fntpe = defn.PolyFunctionOf(mt)
        if !encl.isEmpty then
          val cs = CaptureSet(encl.map(_.paramRefs.head)*)
          CapturingType(fntpe, cs, boxed = false)
        else fntpe

      /** 1. Check that parents of capturing types are not pure.
       */
      def checkRetainsOK(tp: Type): tp.type =
        tp match
          case CapturingType(parent, refs) =>
            if parent.isAlwaysPure && !tptToCheck.span.isZeroExtent then
              // If tptToCheck is zero-extent it could be copied from an overridden
              // method's result type. In that case, there's no point requiring
              // an explicit result type in the override, the inherited capture set
              // will be ignored anyway.
              fail(em"$parent is a pure type, it makes no sense to add a capture set to it")
          case _ =>
        tp

      /** Map references to capability classes C to C^{cap.rd},
       *  normalize captures and map to dependent functions.
       */
      def defaultApply(t: Type) =
        if t.derivesFromCapability
          && t.typeParams.isEmpty
          && !t.isSingleton
          && (!sym.isConstructor || (t ne tp.finalResultType))
            // Don't add ^ to result types of class constructors deriving from Capability
        then
          normalizeCaptures(mapOver(t)) match
            case t1 @ CapturingType(_, _) => t1
            case t1 => CapturingType(t1, CaptureSet.CSImpliedByCapability(t1, sym, variance), boxed = false)
        else normalizeCaptures(mapFollowingAliases(t))

      def innerApply(t: Type) =
        t match
          case t @ CapturingType(parent, refs) =>
            checkRetainsOK:
              t.derivedCapturingType(stripImpliedCaptureSet(this(parent)), refs)
          case t @ AnnotatedType(parent, ann: RetainingAnnotation) if ann.isStrict =>
            val parent1 = stripImpliedCaptureSet(this(parent))
            if !tptToCheck.isEmpty then
              checkWellformedLater(parent1, ann, tptToCheck)
            try
              checkRetainsOK:
                CapturingType(parent1, ann.toCaptureSet)
            catch case ex: IllegalCaptureRef =>
              if !tptToCheck.isEmpty then
                report.error(em"Illegal capture reference: ${ex.getMessage}", tptToCheck.srcPos)
              parent1
          case t @ AnnotatedType(parent, ann) =>
            if ann.symbol == defn.UncheckedCapturesAnnot
            then makeUnchecked(this(parent))
            else t.derivedAnnotatedType(this(parent), ann)
          case throwsAlias(res, exc) =>
            this(expandThrowsAlias(res, exc, Nil))
          case t @ AppliedType(tycon, args)
          if defn.isNonRefinedFunction(t)
              && !defn.isFunctionSymbol(t.typeSymbol) && (t.dealias ne tp) =>
            if keepFunAliases then
              // Hold off with dealising and expand in a second pass.
              // This is necessary to bind existentialFresh instances to the right method binder.
              keptFunAliases = true
              mapOver(t)
            else
              // In the second pass, map the alias
              apply(t.dealias)
          case t =>
            defaultApply(t)
    end toCapturing

    def transform(tp: Type): Type =
      val tp1 = toCapturing(tp)
      val tp2 = toResultInResults(sym, fail, toCapturing.keepFunAliases)(tp1)
      val snd = if toCapturing.keepFunAliases then "" else " 2nd time"
      if tp2 ne tp then capt.println(i"expanded explicit$snd in ${ctx.owner}: $tp  -->  $tp1  -->  $tp2")
      tp2

    val tp1 = transform(tp)
    val tp2 =
      if toCapturing.keptFunAliases then
        toCapturing.keepFunAliases = false
        transform(tp1)
      else tp1
    val tp3 =
      if sym.isType then stripImpliedCaptureSet(tp2)
      else tp2
    capToFresh(tp3, Origin.InDecl(sym))
  end transformExplicitType

  /** Update info of `sym` for CheckCaptures phase only */
  private def updateInfo(sym: Symbol, info: Type, owner: Symbol)(using Context) =
    toBeUpdated += sym
    sym.updateInfo(thisPhase, info, newFlagsFor(sym), owner)
    toBeUpdated -= sym

  /** The info of `sym` at the CheckCaptures phase */
  extension (sym: Symbol) def nextInfo(using Context): Type =
    atPhase(thisPhase.next)(sym.info)

  val fieldsWithExplicitTypes: mutable.HashMap[ClassSymbol, List[Symbol]] = mutable.HashMap()

  val capturedBy: mutable.HashMap[Symbol, Symbol] = mutable.HashMap()

  val anonFunCallee: mutable.HashMap[Symbol, Symbol] = mutable.HashMap()

  /** A traverser that adds knownTypes and updates symbol infos */
  def setupTraverser(checker: CheckerAPI) = new TreeTraverserWithPreciseImportContexts:
    import checker.*

    /** Transform type of tree, and remember the transformed type as the type of the tree
     *  @pre !(boxed && sym.exists)
     */
    private def transformTT(tree: TypeTree, sym: Symbol, boxed: Boolean)(using Context): Unit =
      if !tree.hasNuType then
        var transformed =
          if tree.isInferred || sym.is(ModuleVal)
          then transformInferredType(tree.tpe)
          else transformExplicitType(tree.tpe, sym, tptToCheck = tree)
        if boxed then transformed = transformed.boxDeeply
        tree.setNuType(
          if sym.hasAnnotation(defn.UncheckedCapturesAnnot) then makeUnchecked(transformed)
          else transformed)

    /** Transform the type of a val or var or the result type of a def */
    def transformResultType(tpt: TypeTree, sym: Symbol)(using Context): Unit =
      // First step: Transform the type and record it as knownType of tpt.
      try
        inContext(ctx.addMode(Mode.CCPreciseOwner)):
          transformTT(tpt, sym, boxed = false)
      catch case ex: IllegalCaptureRef =>
        capt.println(i"fail while transforming result type $tpt of $sym")
        throw ex

      // Second step: Add descriptions for all capture set variables created in
      // step one stating that they belong to `sym`.
      val addDescription = new TypeTraverser:
        def traverse(tp: Type) = tp match
          case tp @ CapturingType(parent, refs) =>
            if !refs.isConst && refs.description.isEmpty then
              refs.withDescription(i"of $sym")
            traverse(parent)
          case _ =>
            traverseChildren(tp)
      addDescription.traverse(tpt.nuType)
    end transformResultType

    def traverse(tree: Tree)(using Context): Unit =
      tree match
        case tree: Ident =>
          val sym = tree.symbol
          if sym.isMutableVar && sym.owner.isTerm then
            val enclMeth = ctx.owner.enclosingMethod
            if sym.enclosingMethod != enclMeth then
              capturedBy(sym) = enclMeth

        case Apply(fn, args) =>
          for case closureDef(mdef) <- args do
            anonFunCallee(mdef.symbol) = fn.symbol
          traverseChildren(tree)

        case tree @ DefDef(_, paramss, tpt: TypeTree, _) =>
          val meth = tree.symbol
          if isExcluded(meth) then
            return

          inContext(ctx.withOwner(meth)):
            paramss.foreach(traverse)
            transformResultType(tpt, meth)
            traverse(tree.rhs)

        case tree @ ValDef(_, tpt: TypeTree, _) =>
          val sym = tree.symbol
          val defCtx = if sym.isOneOf(TermParamOrAccessor) then ctx else ctx.withOwner(sym)
          inContext(defCtx):
            transformResultType(tpt, sym)
            traverse(tree.rhs)

        case tree @ TypeApply(fn, args) =>
          traverse(fn)
          for case arg: TypeTree <- args do
            if defn.isTypeTestOrCast(fn.symbol) then
              arg.setNuType(
                capToFresh(arg.tpe, Origin.TypeArg(arg.tpe)))
            else
              transformTT(arg, NoSymbol, boxed = true) // type arguments in type applications are boxed

        case tree: TypeDef if tree.symbol.isClass =>
          val sym = tree.symbol
          inContext(ctx.withOwner(sym))
            traverseChildren(tree)

        case tree @ TypeDef(_, rhs: TypeTree) =>
          transformTT(rhs, tree.symbol, boxed = false)

        case tree @ SeqLiteral(elems, tpt: TypeTree) =>
          traverse(elems)
          tpt.setNuType(transformInferredType(tpt.tpe).boxDeeply)

        case tree @ Try(body, catches, finalizer) =>
          val tryOwner = firstCanThrowEvidence(body) match
            case Some(vd) =>
              newSymbol(ctx.owner, TryOwnerName.fresh(),
                Method | Synthetic, ExprType(defn.NothingType), coord = tree.span)
            case _ =>
              ctx.owner
          inContext(ctx.withOwner(tryOwner)):
            traverse(body)
          catches.foreach(traverse)
          traverse(finalizer)

        case tree: New =>

        case _ =>
          traverseChildren(tree)

      postProcess(tree)
      checkProperUseOrConsume(tree)
    end traverse

    /** Processing done on node `tree` after its children are traversed */
    def postProcess(tree: Tree)(using Context): Unit = tree match
      case tree: TypeTree =>
        transformTT(tree, NoSymbol, boxed = false)
      case tree: ValOrDefDef =>
        // Make sure denotation of tree's symbol is correct
        val sym = tree.symbol

        // The return type of a constructor instantiated with local type and value
        // parameters. Constructor defs have `Unit` as result type tree, that's why
        // we can't get this type by reading the result type tree, and have to construct
        // it explicitly.
        def constrReturnType(info: Type, psymss: List[List[Symbol]]): Type = info match
          case info: MethodOrPoly =>
            constrReturnType(info.instantiate(psymss.head.map(_.namedType)), psymss.tail)
          case _ =>
            info

        // The local result type, which is the known type of the result type tree,
        // with special treatment for constructors.
        def localReturnType =
          if sym.isConstructor then constrReturnType(sym.info, sym.paramSymss)
          else tree.tpt.nuType

        // A test whether parameter signature might change. This returns true if one of
        // the parameters has a new type installed. The idea here is that we store a new
        // type only if the transformed type is different from the original.
        def paramSignatureChanges = tree.match
          case tree: DefDef =>
            tree.paramss.nestedExists:
              case param: ValDef =>  param.tpt.hasNuType
              case param: TypeDef => param.rhs.hasNuType
          case _ => false

        // A symbol's signature changes if some of its parameter types or its result type
        // have a new type installed here (meaning hasRememberedType is true)
        def signatureChanges =
          tree.tpt.hasNuType || paramSignatureChanges
        def ownerChanges =
          ctx.owner.name.is(TryOwnerName)

        def paramsToCap(psymss: List[List[Symbol]], mt: Type)(using Context): Type = mt match
          case mt: MethodType =>
            try
              mt.derivedLambdaType(
                paramInfos =
                  psymss.head.lazyZip(mt.paramInfos).map(freshToCap),
                resType = paramsToCap(psymss.tail, mt.resType))
            catch case ex: AssertionError =>
              println(i"error while mapping params ${mt.paramInfos} of $sym")
              throw ex
          case mt: PolyType =>
            mt.derivedLambdaType(resType = paramsToCap(psymss.tail, mt.resType))
          case _ => mt

        // If there's a change in the signature or owner, update the info of `sym`
        if sym.exists && (signatureChanges || ownerChanges) then
          val updatedInfo =
            if signatureChanges then
              val paramSymss = sym.paramSymss
              def newInfo(using Context) = // will be run in this or next phase
                toResultInResults(sym, report.error(_, tree.srcPos)):
                  if sym.is(Method) then
                    inContext(ctx.withOwner(sym)):
                      paramsToCap(paramSymss, methodType(paramSymss, localReturnType))
                  else tree.tpt.nuType
              if tree.tpt.isInstanceOf[InferredTypeTree]
                  && !sym.is(Param) && !sym.is(ParamAccessor)
              then
                val prevInfo = sym.info
                new LazyType:
                  def complete(denot: SymDenotation)(using Context) =
                    assert(ctx.phase == thisPhase.next, i"$sym")
                    sym.info = prevInfo // set info provisionally so we can analyze the symbol in recheck
                    completeDef(tree, sym, this)
                    sym.info = newInfo
                      .showing(i"new info of $sym = $result", capt)
              else if sym.is(Method) then
                new LazyType:
                  def complete(denot: SymDenotation)(using Context) =
                    sym.info = newInfo
                      .showing(i"new info of $sym = $result", capt)
              else newInfo
            else sym.info
          val updatedOwner = if ownerChanges then ctx.owner else sym.owner
          updateInfo(sym, updatedInfo, updatedOwner)

      case tree: Bind =>
        val sym = tree.symbol
        updateInfo(sym, transformInferredType(sym.info), sym.owner)
      case tree @ TypeDef(_, impl: Template) =>
        val cls: ClassSymbol = tree.symbol.asClass

        fieldsWithExplicitTypes(cls) =
          for
            case vd @ ValDef(_, tpt: TypeTree, _) <- impl.body
            if !tpt.isInferred && vd.symbol.exists && !vd.symbol.is(NonMember)
          yield
            vd.symbol

        checkClassifiedInheritance(cls)
        val cinfo @ ClassInfo(prefix, _, ps, decls, selfInfo) = cls.classInfo

        // Compute new self type
        val selfInfo1 =
          if (selfInfo ne NoType) && !cls.is(ModuleClass) then
            // if selfInfo is explicitly given then use that one, except if
            // self info applies to a module class, these still need to be inferred
            selfInfo
          else if cls.isPureClass then
            // is cls is known to be pure, nothing needs to be added to self type
            selfInfo
          else if !cls.isEffectivelySealed && !cls.baseClassHasExplicitNonUniversalSelfType then
            // assume {cap} for completely unconstrained self types of publicly extensible classes
            CapturingType(cinfo.selfType, CaptureSet.universal)
          else {
            // Infer the self type for the rest, which is all classes without explicit
            // self types (to which we also add nested module classes), provided they are
            // neither pure, nor are publicily extensible with an unconstrained self type.
            val cs = CaptureSet.ProperVar(cls, CaptureSet.emptyRefs, nestedOK = false, isRefining = false)

            if cls.derivesFrom(defn.Caps_Capability) then
              // If cls is a capability class, we need to add a fresh capability to ensure
              // we cannot treat the class as pure.
              CaptureSet.fresh(cls, cls.thisType, Origin.InDecl(cls)).subCaptures(cs)

            // Add capture set variable `cs`, burying it under any refinements
            // that might contain infos of opaque type aliases
            def addCs(tp: Type): Type = tp match
              case tp @ RefinedType(parent, _, _) => tp.derivedRefinedType(parent = addCs(parent))
              case _ => CapturingType(tp, cs)
            addCs(cinfo.selfType)
          }

        // Compute new parent types
        val ps1 = inContext(ctx.withOwner(cls)):
          ps.mapConserve(transformExplicitType(_, NoSymbol))

        // Install new types and if it is a module class also update module object
        if (selfInfo1 ne selfInfo) || (ps1 ne ps) then
          val newInfo = ClassInfo(prefix, cls, ps1, decls, selfInfo1)
          updateInfo(cls, newInfo, cls.owner)
          capt.println(i"update class info of $cls with parents $ps selfinfo $selfInfo to $newInfo")
          cls.thisType.asInstanceOf[ThisType].invalidateCaches()
      case _ =>
    end postProcess

    /** Check that @use and @consume annotations only appear on parameters and not on
     *  anonymous function parameters. Check that @use annotations don't appear
     *  at all from 3.8 on.
     */
    def checkProperUseOrConsume(tree: Tree)(using Context): Unit = tree match
      case tree: MemberDef =>
        val sym = tree.symbol
        def isMethodParam = (sym.is(Param) || sym.is(ParamAccessor))
            && !sym.owner.isAnonymousFunction
        for ann <- tree.symbol.annotations do
          val annotCls = ann.symbol
          if annotCls == defn.ConsumeAnnot then
            if !(isMethodParam && sym.isTerm)
              && !(sym.is(Method) && sym.owner.isClass)
            then
              report.error(
                em"""consume cannot be used here. Only member methods and their term parameters
                    |can have a consume modifier.""",
                tree.srcPos)
          else if annotCls == defn.UseAnnot then
            if !ccConfig.allowUse then
              if sym.is(TypeParam) then
                report.error(
                  em"""@use is redundant here and should no longer be written explicitly.
                      |Capset variables are always implicitly used, unless they are annotated with @caps.preserve.""",
                  tree.srcPos)
              else
                report.error(
                  em"""@use is no longer supported. Instead of @use you can introduce capset
                      |variables for the polymorphic parts of parameter types.""",
                  tree.srcPos)
            else if !isMethodParam then
              report.error(
                em"@use cannot be used here. Only method parameters can have @use annotations.",
                tree.srcPos)
      case _ =>
    end checkProperUseOrConsume
  end setupTraverser

// --------------- Adding capture set variables ----------------------------------

  /** Checks whether an abstract type or its bound could be impure. If that's the case,
   *  the abstract type gets a capture set variable in `decorate`.
   *  See also: [[needsVariable]].
   */
  private def instanceCanBeImpure(tp: Type)(using Context): Boolean = {
    tp.dealiasKeepAnnots match
      case CapturingOrRetainsType(_, refs) =>
        !refs.isAlwaysEmpty
      case tp: (TypeRef | AppliedType) =>
        val sym = tp.typeSymbol
        if sym.isClass
        then !sym.isPureClass
        else !tp.derivesFrom(defn.Caps_CapSet) // CapSet arguments don't get other capture set variables added
          && instanceCanBeImpure(tp.superType)
      case tp: (RefinedOrRecType | MatchType) =>
        instanceCanBeImpure(tp.underlying)
      case tp: AndType =>
        instanceCanBeImpure(tp.tp1) || instanceCanBeImpure(tp.tp2)
      case tp: OrType =>
        instanceCanBeImpure(tp.tp1) && instanceCanBeImpure(tp.tp2)
      case _ =>
        false
  }.showing(i"instance can be impure $tp = $result", capt)

  /** Should a capture set variable be added on type `tp`?
   *  Notable exceptions are:
   *   - If type refers to a class which is Pure, or it is Any, it does not need a variable.
   *   - If type is an abstract or parameter type we decide according to `instanceCanBeImpure`
   *   - If type is a capturing type that has already a capture set variable or has
   *     the universal capture set, it does not need a variable.
   */
  def needsVariable(tp: Type)(using Context): Boolean = {
    tp.typeParams.isEmpty && tp.match
      case tp: (TypeRef | AppliedType) =>
        val sym = tp.typeSymbol
        if sym.isClass then
          !sym.isPureClass && sym != defn.AnyClass
        else
          val tp1 = tp.dealiasKeepAnnotsAndOpaques
          if tp1 ne tp then needsVariable(tp1)
          else instanceCanBeImpure(tp1)
      case tp: (RefinedOrRecType | MatchType) =>
        needsVariable(tp.underlying)
      case tp: AndType =>
        needsVariable(tp.tp1) && needsVariable(tp.tp2)
      case tp: OrType =>
        needsVariable(tp.tp1) || needsVariable(tp.tp2)
      case CapturingOrRetainsType(parent, refs) =>
        needsVariable(parent)
        && refs.isConst       // if refs is a variable, no need to add another
        && !refs.isUniversal  // if refs is {cap}, an added variable would not change anything
      case AnnotatedType(parent, _) =>
        needsVariable(parent)
      case _ =>
        false
  }.showing(i"can have inferred capture $tp = $result", captDebug)

  /** Add a capture set variable or <fluid> set to `tp` if necessary.
   *  Dealias `tp` (but keep annotations and opaque types) if doing
   *  so ends up adding a capture set.
   *  @param tp     the type to add a capture set to
   *  @param added  A function producing the added capture set from a set of initial elements.
   */
  def decorate(tp: Type, added: CaptureSet.Refs => CaptureSet)(using Context): Type =
    if tp.typeSymbol == defn.FromJavaObjectSymbol then
      // For capture checking, we assume Object from Java is the same as Any
      tp
    else
      def maybeAdd(target: Type, fallback: Type) =
        if needsVariable(target) then
          target match
            case CapturingType(_, CaptureSet.Fluid) =>
              target
            case CapturingType(target1, cs1) if cs1.isConst =>
              CapturingType(target1, added(cs1.elems))
            case _ =>
              CapturingType(target, added(SimpleIdentitySet.empty))
        else fallback
      val dealiased = tp.dealiasKeepAnnotsAndOpaques
      if dealiased ne tp then
        val transformed = transformInferredType(dealiased)
        maybeAdd(transformed, if transformed ne dealiased then transformed else tp)
      else maybeAdd(tp, tp)

  /** Add a capture set variable to `tp` if necessary. */
  private def addVar(tp: Type, owner: Symbol, isRefining: Boolean)(using Context): Type =
    decorate(tp, CaptureSet.ProperVar(owner, _, nestedOK = !ctx.mode.is(Mode.CCPreciseOwner), isRefining))

  /** A map that adds <fluid> capture sets at all contra- and invariant positions
   *  in a type where a capture set would be needed. This is used to make types
   *  that were not capture checked compatible with types that are capture checked.
   *  We don't need to add <fluid> in covariant positions since pure types are
   *  anyway compatible with capturing types.
   */
  private def fluidify(using Context) = new TypeMap:
    def apply(t: Type): Type = t match
      case t: MethodType =>
        mapOver(t)
      case t: TypeLambda =>
        t.derivedLambdaType(resType = this(t.resType))
      case CapturingType(_, _) =>
        t
      case _ =>
        val t1  = t match
          case t @ defn.RefinedFunctionOf(rinfo: MethodType) =>
            // Just refine the apply method, don't bother with the parent
            t.derivedRefinedType(t.parent, t.refinedName, this(rinfo))
          case _ =>
            mapOver(t)
        if variance > 0 then t1
        else decorate(t1, Function.const(CaptureSet.Fluid))

  /** Replace all universal capture sets in this type by <fluid> */
  private def makeUnchecked(using Context): TypeMap = new TypeMap with FollowAliasesMap:
    def apply(t: Type) = t match
      case t @ CapturingType(parent, refs) =>
        val parent1 = this(parent)
        if refs.containsTerminalCapability then t.derivedCapturingType(parent1, CaptureSet.Fluid)
        else t
      case _ => mapFollowingAliases(t)

  /** Run setup on a compilation unit with given `tree`.
   *  @param recheckDef   the function to run for completing a val or def
   */
  def setupUnit(tree: Tree, checker: CheckerAPI)(using Context): Unit =
    setupTraverser(checker).traverse(tree)(using ctx.withPhase(thisPhase))

  // ------ Checks to run at Setup ----------------------------------------

  private def checkClassifiedInheritance(cls: ClassSymbol)(using Context): Unit =
    def recur(cs: List[ClassSymbol]): Unit = cs match
      case c :: cs1 =>
        for c1 <- cs1 do
          if !c.derivesFrom(c1) && !c1.derivesFrom(c) then
            report.error(em"$cls inherits two unrelated classifier traits: $c and $c1", cls.srcPos)
        recur(cs1)
      case Nil =>
    recur(cls.baseClasses.filter(_.isClassifiedCapabilityClass).distinct)
    if cls.derivesFrom(defn.Caps_SharedCapability) && cls.derivesFrom(defn.Caps_Stateful) then
      report.error(em"$cls cannot inherit from both SharedCapability and Stateful", cls.srcPos)

  // ------ Checks to run after main capture checking --------------------------

  /** A list of actions to perform at postCheck */
  private val todoAtPostCheck = new mutable.ListBuffer[Context => Unit]

  /** If `tp` is a capturing type, check that all references it mentions have non-empty
   *  capture sets.
   *  Also: warn about redundant capture annotations.
   *  This check is performed after capture sets are computed in phase cc.
   *  Note: We need to perform the check on the original annotation rather than its
   *  capture set since the conversion to a capture set already eliminates redundant elements.
   *  @param parent   the parent of the capturing type
   *  @param ann      the original retains annotation
   *  @param tpt      the tree for which an error or warning should be reported
   */
  private def checkWellformed(parent: Type, ann: RetainingAnnotation, tpt: Tree)(using Context): Unit =
    capt.println(i"checkWF post $parent ${ann.retainedType} in $tpt")
    try
      var retained = ann.retainedType.retainedElements.toArray
      for i <- 0 until retained.length do
        val ref = retained(i)
        def pos = tpt.srcPos

        def check(others: CaptureSet, dom: Type | CaptureSet): Unit =
          if others.accountsFor(ref) then
            report.warning(em"redundant capture: $dom already accounts for $ref", pos)

        if !ref.coreType.derivesFrom(defn.Caps_Capability)
            // Capability classes don't have their implied capture set yet, so
            // they would be seen as pure
            && !ref.coreType.derivesFrom(defn.Caps_CapSet)
        then
          if ref.captureSetOfInfo.elems.isEmpty then
            val deepStr = if ref.isReach then " deep" else ""
            report.error(em"$ref cannot be tracked since its$deepStr capture set is empty", pos)
          check(parent.captureSet, parent)

          val others =
            for
              j <- 0 until retained.length if j != i
              r = retained(j)
              if !r.isTerminalCapability
            yield r
          val remaining = CaptureSet(others*)
          check(remaining, remaining)
      end for
    catch case ex: IllegalCaptureRef =>
      report.error(em"Illegal capture reference: ${ex.getMessage}", tpt.srcPos)
  end checkWellformed

  /** Check well formed at post check time. We need to wait until after
   *  recheck because we find out only then whether capture sets are empty or
   *  capabilities are redundant.
   */
  private def checkWellformedLater(parent: Type, ann: RetainingAnnotation, tpt: Tree)(using Context): Unit =
    if !tpt.span.isZeroExtent && enclosingInlineds.isEmpty then
      todoAtPostCheck += (ctx1 =>
        checkWellformed(parent, ann, tpt)(using ctx1.withOwner(ctx.owner)))

  /** Run all well formedness tests that were deferred to post check */
  def postCheck()(using Context): Unit =
    for chk <- todoAtPostCheck do chk(ctx)
    todoAtPostCheck.clear()
end Setup
