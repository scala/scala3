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
import CaptureSet.{IdentityCaptRefMap, IdempotentCaptRefMap}
import Synthetics.isExcluded
import util.SimpleIdentitySet
import reporting.Message
import printing.{Printer, Texts}, Texts.{Text, Str}
import collection.mutable
import CCState.*
import dotty.tools.dotc.util.NoSourcePosition
import CheckCaptures.CheckerAPI

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
   *  The @retains mught be implicit for a type deriving from `Capability`.
   */
  private def newFlagsFor(symd: SymDenotation)(using Context): FlagSet =

    object containsCovarRetains extends TypeAccumulator[Boolean]:
      val seen = util.HashSet[Symbol]()
      def apply(x: Boolean, tp: Type): Boolean =
        if x then true
        else if tp.derivesFromCapability && variance >= 0 then true
        else tp match
          case AnnotatedType(_, ann) if ann.symbol.isRetains && variance >= 0 => true
          case t: TypeRef if t.symbol.isAbstractOrParamType && !seen.contains(t.symbol) =>
            seen += t.symbol
            apply(x, t.info.bounds.hi)
          case _ => foldOver(x, tp)
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
        if toBeUpdated.contains(sym)
        then symd.info // don't transform symbols that will anyway be updated
        else transformExplicitType(symd.info)
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

  /** If `tp` is an unboxed capturing type or a function returning an unboxed capturing type,
   *  convert it to be boxed.
   */
  private def box(tp: Type)(using Context): Type =
    def recur(tp: Type): Type = tp.dealiasKeepAnnotsAndOpaques match
      case tp @ CapturingType(parent, refs) =>
        if tp.isBoxed || parent.derivesFrom(defn.Caps_CapSet) then tp
        else tp.boxed
      case tp @ AnnotatedType(parent, ann) =>
        if ann.symbol.isRetains && !parent.derivesFrom(defn.Caps_CapSet)
        then CapturingType(parent, ann.tree.toCaptureSet, boxed = true)
        else tp.derivedAnnotatedType(box(parent), ann)
      case tp1 @ AppliedType(tycon, args) if defn.isNonRefinedFunction(tp1) =>
        val res = args.last
        val boxedRes = recur(res)
        if boxedRes eq res then tp
        else tp1.derivedAppliedType(tycon, args.init :+ boxedRes)
      case tp1 @ defn.RefinedFunctionOf(rinfo: MethodType) =>
        val boxedRinfo = recur(rinfo)
        if boxedRinfo eq rinfo then tp
        else boxedRinfo.toFunctionType(alwaysDependent = true)
      case tp1: MethodOrPoly =>
        val res = tp1.resType
        val boxedRes = recur(res)
        if boxedRes eq res then tp
        else tp1.derivedLambdaType(resType = boxedRes)
      case _ => tp
    tp match
      case tp: MethodOrPoly => tp // don't box results of methods outside refinements
      case _ => recur(tp)

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
    def mapInferred(refine: Boolean): TypeMap = new TypeMap with FollowAliasesMap:
      override def toString = "map inferred"

      /** Refine a possibly applied class type C where the class has tracked parameters
       *  x_1: T_1, ..., x_n: T_n to C { val x_1: T_1^{CV_1}, ..., val x_n: T_n^{CV_n} }
       *  where CV_1, ..., CV_n are fresh capture set variables.
       */
      def addCaptureRefinements(tp: Type): Type = tp match
        case _: TypeRef | _: AppliedType if refine && tp.typeParams.isEmpty =>
          tp.typeSymbol match
            case cls: ClassSymbol
            if !defn.isFunctionClass(cls) && cls.is(CaptureChecked) =>
              cls.paramGetters.foldLeft(tp) { (core, getter) =>
                if atPhase(thisPhase.next)(getter.hasTrackedParts)
                    && getter.isRefiningParamAccessor
                    && !getter.is(Tracked)
                then
                  val getterType =
                    mapInferred(refine = false)(tp.memberInfo(getter)).strippedDealias
                  RefinedType(core, getter.name,
                      CapturingType(getterType, new CaptureSet.RefiningVar(ctx.owner)))
                    .showing(i"add capture refinement $tp --> $result", capt)
                else
                  core
              }
            case _ => tp
        case _ => tp

      private var isTopLevel = true

      private def mapNested(ts: List[Type]): List[Type] =
        val saved = isTopLevel
        isTopLevel = false
        try ts.mapConserve(this)
        finally isTopLevel = saved

      def apply(tp: Type) =
        val tp1 = tp match
          case AnnotatedType(parent, annot) if annot.symbol.isRetains =>
            // Drop explicit retains annotations
            apply(parent)
          case tp @ AppliedType(tycon, args) =>
            val tycon1 = this(tycon)
            if defn.isNonRefinedFunction(tp) then
              // Convert toplevel generic function types to dependent functions
              if !defn.isFunctionSymbol(tp.typeSymbol) && (tp.dealias ne tp) then
                // This type is a function after dealiasing, so we dealias and recurse.
                // See #15925.
                this(tp.dealias)
              else
                val args0 = args.init
                var res0 = args.last
                val args1 = mapNested(args0)
                val res1 = this(res0)
                if isTopLevel then
                  depFun(args1, res1,
                      isContextual = defn.isContextFunctionClass(tycon1.classSymbol))
                    .showing(i"add function refinement $tp ($tycon1, $args1, $res1) (${tp.dealias}) --> $result", capt)
                else if (tycon1 eq tycon) && (args1 eq args0) && (res1 eq res0) then
                  tp
                else
                  tp.derivedAppliedType(tycon1, args1 :+ res1)
            else
              tp.derivedAppliedType(tycon1, args.mapConserve(arg => box(this(arg))))
          case defn.RefinedFunctionOf(rinfo: MethodType) =>
            val rinfo1 = apply(rinfo)
            if rinfo1 ne rinfo then rinfo1.toFunctionType(alwaysDependent = true)
            else tp
          case Existential(_, unpacked) =>
            // drop the existential, the bound variables will be replaced by capture set variables
            apply(unpacked)
          case tp: MethodType =>
            tp.derivedLambdaType(
              paramInfos = mapNested(tp.paramInfos),
              resType = this(tp.resType))
          case tp: TypeLambda =>
            // Don't recurse into parameter bounds, just cleanup any stray retains annotations
            tp.derivedLambdaType(
              paramInfos = tp.paramInfos.mapConserve(_.dropAllRetains.bounds),
              resType = this(tp.resType))
          case _ =>
            mapFollowingAliases(tp)
        addVar(addCaptureRefinements(normalizeCaptures(tp1)), ctx.owner)
      end apply
    end mapInferred

    try
      val tp1 = mapInferred(refine = true)(tp)
      val tp2 = Existential.mapCapInResults(_ => assert(false))(tp1)
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
   *  6. Perform normalizeCaptures
   */
  private def transformExplicitType(tp: Type, tptToCheck: Tree = EmptyTree)(using Context): Type =
    val toCapturing = new DeepTypeMap with FollowAliasesMap:
      override def toString = "expand aliases"

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

      /** If C derives from Capability and we have a C^cs in source, we leave it as is
       *  instead of expanding it to C^{cap}^cs. We do this by stripping capability-generated
       *  universal capture sets from the parent of a CapturingType.
       */
      def stripImpliedCaptureSet(tp: Type): Type = tp match
        case tp @ CapturingType(parent, refs)
        if (refs eq defn.universalCSImpliedByCapability) && !tp.isBoxedCapturing =>
          parent
        case _ => tp

      def apply(t: Type) =
        t match
          case t @ CapturingType(parent, refs) =>
            t.derivedCapturingType(stripImpliedCaptureSet(this(parent)), refs)
          case t @ AnnotatedType(parent, ann) =>
            val parent1 = this(parent)
            if ann.symbol.isRetains then
              val parent2 = stripImpliedCaptureSet(parent1)
              if !tptToCheck.isEmpty then
                checkWellformedLater(parent2, ann.tree, tptToCheck)
              try
                CapturingType(parent2, ann.tree.toCaptureSet)
              catch case ex: IllegalCaptureRef =>
                report.error(em"Illegal capture reference: ${ex.getMessage.nn}", tptToCheck.srcPos)
                parent2
            else
              t.derivedAnnotatedType(parent1, ann)
          case throwsAlias(res, exc) =>
            this(expandThrowsAlias(res, exc, Nil))
          case t =>
            // Map references to capability classes C to C^
            if t.derivesFromCapability && !t.isSingleton && t.typeSymbol != defn.Caps_Exists
            then CapturingType(t, defn.universalCSImpliedByCapability, boxed = false)
            else normalizeCaptures(mapFollowingAliases(t))
    end toCapturing

    def fail(msg: Message) =
      if !tptToCheck.isEmpty then report.error(msg, tptToCheck.srcPos)

    val tp1 = toCapturing(tp)
    val tp2 = Existential.mapCapInResults(fail)(tp1)
    if tp2 ne tp then capt.println(i"expanded explicit in ${ctx.owner}: $tp  -->  $tp1  -->  $tp2")
    tp2
  end transformExplicitType

  /** Substitute parameter symbols in `from` to paramRefs in corresponding
   *  method or poly types `to`. We use a single BiTypeMap to do everything.
   *  @param from  a list of lists of type or term parameter symbols of a curried method
   *  @param to    a list of method or poly types corresponding one-to-one to the parameter lists
   */
  private class SubstParams(from: List[List[Symbol]], to: List[LambdaType])(using Context)
  extends DeepTypeMap, BiTypeMap:

    def apply(t: Type): Type = t match
      case t: NamedType =>
        if t.prefix == NoPrefix then
          val sym = t.symbol
          def outer(froms: List[List[Symbol]], tos: List[LambdaType]): Type =
            def inner(from: List[Symbol], to: List[ParamRef]): Type =
              if from.isEmpty then outer(froms.tail, tos.tail)
              else if sym eq from.head then to.head
              else inner(from.tail, to.tail)
            if tos.isEmpty then t
            else inner(froms.head, tos.head.paramRefs)
          outer(from, to)
        else t.derivedSelect(apply(t.prefix))
      case _ =>
        mapOver(t)

    lazy val inverse = new BiTypeMap:
      override def toString = "SubstParams.inverse"
      def apply(t: Type): Type = t match
        case t: ParamRef =>
          def recur(from: List[LambdaType], to: List[List[Symbol]]): Type =
            if from.isEmpty then t
            else if t.binder eq from.head then to.head(t.paramNum).namedType
            else recur(from.tail, to.tail)
          recur(to, from)
        case _ =>
          mapOver(t)
      def inverse = SubstParams.this
  end SubstParams

  /** Update info of `sym` for CheckCaptures phase only */
  private def updateInfo(sym: Symbol, info: Type)(using Context) =
    toBeUpdated += sym
    sym.updateInfo(thisPhase, info, newFlagsFor(sym))
    toBeUpdated -= sym

  /** The info of `sym` at the CheckCaptures phase */
  extension (sym: Symbol) def nextInfo(using Context): Type =
    atPhase(thisPhase.next)(sym.info)

  /** A traverser that adds knownTypes and updates symbol infos */
  def setupTraverser(checker: CheckerAPI) = new TreeTraverserWithPreciseImportContexts:
    import checker.*

    /** Transform type of tree, and remember the transformed type as the type the tree */
    private def transformTT(tree: TypeTree, boxed: Boolean)(using Context): Unit =
      if !tree.hasNuType then
        val transformed =
          if tree.isInferred
          then transformInferredType(tree.tpe)
          else transformExplicitType(tree.tpe, tptToCheck = tree)
        tree.setNuType(if boxed then box(transformed) else transformed)

    /** Transform the type of a val or var or the result type of a def */
    def transformResultType(tpt: TypeTree, sym: Symbol)(using Context): Unit =
      // First step: Transform the type and record it as knownType of tpt.
      try
        transformTT(tpt,
            boxed =
              sym.is(Mutable, butNot = Method)
                && !ccConfig.useSealed
                && !sym.hasAnnotation(defn.UncheckedCapturesAnnot),
              // Under the sealed policy, we disallow root capabilities in the type of mutable
              // variables, no need to box them here.
          )
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
        case tree @ DefDef(_, paramss, tpt: TypeTree, _) =>
          val meth = tree.symbol
          if isExcluded(meth) then
            return

          meth.recordLevel()
          inNestedLevel:
            inContext(ctx.withOwner(meth)):
              paramss.foreach(traverse)
              transformResultType(tpt, meth)
              traverse(tree.rhs)

        case tree @ ValDef(_, tpt: TypeTree, _) =>
          val sym = tree.symbol
          sym.recordLevel()
          val defCtx = if sym.isOneOf(TermParamOrAccessor) then ctx else ctx.withOwner(sym)
          inContext(defCtx):
            transformResultType(tpt, sym)
            traverse(tree.rhs)

        case tree @ TypeApply(fn, args) =>
          traverse(fn)
          if !defn.isTypeTestOrCast(fn.symbol) then
            for case arg: TypeTree <- args do
              transformTT(arg, boxed = true) // type arguments in type applications are boxed

        case tree: TypeDef if tree.symbol.isClass =>
          val sym = tree.symbol
          sym.recordLevel()
          inNestedLevelUnless(sym.is(Module)):
            inContext(ctx.withOwner(sym))
              traverseChildren(tree)

        case tree @ SeqLiteral(elems, tpt: TypeTree) =>
          traverse(elems)
          tpt.setNuType(box(transformInferredType(tpt.tpe)))

        case tree: Block =>
          inNestedLevel(traverseChildren(tree))

        case _ =>
          traverseChildren(tree)
      postProcess(tree)
      checkProperUse(tree)
    end traverse

    /** Processing done on node `tree` after its children are traversed */
    def postProcess(tree: Tree)(using Context): Unit = tree match
      case tree: TypeTree =>
        transformTT(tree, boxed = false)
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
        // the parameters has a new type installee. The idea here is that we store a new
        // type only if the transformed type is different from the original.
        def paramSignatureChanges = tree.match
          case tree: DefDef =>
            tree.paramss.nestedExists:
              case param: ValDef => param.tpt.hasNuType
              case param: TypeDef => param.rhs.hasNuType
          case _ => false

        // A symbol's signature changes if some of its parameter types or its result type
        // have a new type installed here (meaning hasRememberedType is true)
        def signatureChanges =
          tree.tpt.hasNuType && !sym.isConstructor || paramSignatureChanges

        // Replace an existing symbol info with inferred types where capture sets of
        // TypeParamRefs and TermParamRefs are put in correspondence by BiTypeMaps with the
        // capture sets of the types of the method's parameter symbols and result type.
        def integrateRT(
            info: Type,                     // symbol info to replace
            psymss: List[List[Symbol]],     // the local (type and term) parameter symbols corresponding to `info`
            resType: Type,                  // the locally computed return type
            prevPsymss: List[List[Symbol]], // the local parameter symbols seen previously in reverse order
            prevLambdas: List[LambdaType]   // the outer method and polytypes generated previously in reverse order
          ): Type =
          info match
            case mt: MethodOrPoly =>
              val psyms = psymss.head
              // TODO: the substitution does not work for param-dependent method types.
              // For example, `(x: T, y: x.f.type) => Unit`. In this case, when we
              // substitute `x.f.type`, `x` becomes a `TermParamRef`. But the new method
              // type is still under initialization and `paramInfos` is still `null`,
              // so the new `NamedType` will not have a denoation.
              def adaptedInfo(psym: Symbol, info: mt.PInfo): mt.PInfo = mt.companion match
                case mtc: MethodTypeCompanion => mtc.adaptParamInfo(psym, info).asInstanceOf[mt.PInfo]
                case _ => info
              mt.companion(mt.paramNames)(
                mt1 =>
                  if !paramSignatureChanges && !mt.isParamDependent && prevLambdas.isEmpty then
                    mt.paramInfos
                  else
                    val subst = SubstParams(psyms :: prevPsymss, mt1 :: prevLambdas)
                    psyms.map(psym => adaptedInfo(psym, subst(psym.nextInfo).asInstanceOf[mt.PInfo])),
                mt1 =>
                  integrateRT(mt.resType, psymss.tail, resType, psyms :: prevPsymss, mt1 :: prevLambdas)
              )
            case info: ExprType =>
              info.derivedExprType(resType =
                integrateRT(info.resType, psymss, resType, prevPsymss, prevLambdas))
            case info =>
              if prevLambdas.isEmpty then resType
              else SubstParams(prevPsymss, prevLambdas)(resType)

        // If there's a change in the signature, update the info of `sym`
        if sym.exists && signatureChanges then
          val newInfo =
            Existential.mapCapInResults(report.error(_, tree.srcPos)):
              integrateRT(sym.info, sym.paramSymss, localReturnType, Nil, Nil)
            .showing(i"update info $sym: ${sym.info} = $result", capt)
          if newInfo ne sym.info then
            val updatedInfo =
              if sym.isAnonymousFunction
                  || sym.is(Param)
                  || sym.is(ParamAccessor)
                  || sym.isPrimaryConstructor
              then
                // closures are handled specially; the newInfo is constrained from
                // the expected type and only afterwards we recheck the definition
                newInfo
              else new LazyType:
                // infos of other methods are determined from their definitions, which
                // are checked on demand
                def complete(denot: SymDenotation)(using Context) =
                  assert(ctx.phase == thisPhase.next, i"$sym")
                  capt.println(i"forcing $sym, printing = ${ctx.mode.is(Mode.Printing)}")
                  //if ctx.mode.is(Mode.Printing) then new Error().printStackTrace()
                  denot.info = newInfo
                  completeDef(tree, sym)
            updateInfo(sym, updatedInfo)

      case tree: Bind =>
        val sym = tree.symbol
        updateInfo(sym, transformInferredType(sym.info))
      case tree: TypeDef =>
        tree.symbol match
          case cls: ClassSymbol =>
            inNestedLevelUnless(cls.is(Module)):
              val cinfo @ ClassInfo(prefix, _, ps, decls, selfInfo) = cls.classInfo

              // Compute new self type
              def isInnerModule = cls.is(ModuleClass) && !cls.isStatic
              val selfInfo1 =
                if (selfInfo ne NoType) && !isInnerModule then
                  // if selfInfo is explicitly given then use that one, except if
                  // self info applies to non-static modules, these still need to be inferred
                  selfInfo
                else if cls.isPureClass then
                  // is cls is known to be pure, nothing needs to be added to self type
                  selfInfo
                else if !cls.isEffectivelySealed && !cls.baseClassHasExplicitNonUniversalSelfType then
                  // assume {cap} for completely unconstrained self types of publicly extensible classes
                  CapturingType(cinfo.selfType, CaptureSet.universal)
                else
                  // Infer the self type for the rest, which is all classes without explicit
                  // self types (to which we also add nested module classes), provided they are
                  // neither pure, nor are publicily extensible with an unconstrained self type.
                  CapturingType(cinfo.selfType, CaptureSet.Var(cls, level = currentLevel))

              // Compute new parent types
              val ps1 = inContext(ctx.withOwner(cls)):
                ps.mapConserve(transformExplicitType(_))

              // Install new types and if it is a module class also update module object
              if (selfInfo1 ne selfInfo) || (ps1 ne ps) then
                val newInfo = ClassInfo(prefix, cls, ps1, decls, selfInfo1)
                updateInfo(cls, newInfo)
                capt.println(i"update class info of $cls with parents $ps selfinfo $selfInfo to $newInfo")
                cls.thisType.asInstanceOf[ThisType].invalidateCaches()
                if cls.is(ModuleClass) then
                  // if it's a module, the capture set of the module reference is the capture set of the self type
                  val modul = cls.sourceModule
                  updateInfo(modul, CapturingType(modul.info, selfInfo1.asInstanceOf[Type].captureSet))
                  modul.termRef.invalidateCaches()
          case _ =>
      case _ =>
    end postProcess

    /** Check that @use annotations only appear on parameters and not on anonymous function parameters */
    def checkProperUse(tree: Tree)(using Context): Unit = tree match
      case tree: MemberDef =>
        def useAllowed(sym: Symbol) =
          (sym.is(Param) || sym.is(ParamAccessor)) && !sym.owner.isAnonymousFunction
        for ann <- tree.symbol.annotations do
          if ann.symbol == defn.UseAnnot && !useAllowed(tree.symbol) then
            report.error(i"Only parameters of methods can have @use annotations", tree.srcPos)
      case _ =>
    end checkProperUse
  end setupTraverser

// --------------- Adding capture set variables ----------------------------------

  /** Checks whether an abstract type or its bound could be impure. If that's the case,
   *  the abstract type gets a capture set variable in `decorate`.
   *  See also: [[needsVariable]].
   */
  private def instanceCanBeImpure(tp: Type)(using Context): Boolean = {
    tp.dealiasKeepAnnots match
      case CapturingType(_, refs) =>
        !refs.isAlwaysEmpty
      case RetainingType(parent, refs) =>
        !refs.isEmpty
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
      case CapturingType(parent, refs) =>
        needsVariable(parent)
        && refs.isConst       // if refs is a variable, no need to add another
        && !refs.isUniversal  // if refs is {cap}, an added variable would not change anything
      case RetainingType(parent, refs) =>
        needsVariable(parent)
        && !refs.tpes.exists:
            case ref: TermRef => ref.isRootCapability
            case _ => false
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
  private def addVar(tp: Type, owner: Symbol)(using Context): Type =
    decorate(tp, CaptureSet.Var(owner, _, level = currentLevel))

  /** A map that adds <fluid> capture sets at all contra- and invariant positions
   *  in a type where a capture set would be needed. This is used to make types
   *  that were not capture checked compatible with types that are capture checked.
   *  We don't need to add <fluid> in covariant positions since pure types are
   *  anyway compatible with capturing types.
   */
  private def fluidify(using Context) = new TypeMap with IdempotentCaptRefMap:
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
      tp.derivedAppliedType(tycon, args.mapConserve(box))
    case tp: RealTypeBounds =>
      tp.derivedTypeBounds(tp.lo, box(tp.hi))
    case tp: LazyRef =>
      normalizeCaptures(tp.ref)
    case _ =>
      tp

  /** Run setup on a compilation unit with given `tree`.
   *  @param recheckDef   the function to run for completing a val or def
   */
  def setupUnit(tree: Tree, checker: CheckerAPI)(using Context): Unit =
    setupTraverser(checker).traverse(tree)(using ctx.withPhase(thisPhase))

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
  private def checkWellformed(parent: Type, ann: Tree, tpt: Tree)(using Context): Unit =
    capt.println(i"checkWF post $parent ${ann.retainedElems} in $tpt")
    var retained = ann.retainedElems.toArray
    for i <- 0 until retained.length do
      val refTree = retained(i)
      for ref <- refTree.toCaptureRefs do
        def pos =
          if refTree.span.exists then refTree.srcPos
          else if ann.span.exists then ann.srcPos
          else tpt.srcPos

        def check(others: CaptureSet, dom: Type | CaptureSet): Unit =
          if others.accountsFor(ref) then
            report.warning(em"redundant capture: $dom already accounts for $ref", pos)

        if ref.captureSetOfInfo.elems.isEmpty
            && !ref.derivesFrom(defn.Caps_Capability)
            && !ref.derivesFrom(defn.Caps_CapSet) then
          val deepStr = if ref.isReach then " deep" else ""
          report.error(em"$ref cannot be tracked since its$deepStr capture set is empty", pos)
        check(parent.captureSet, parent)

        val others =
          for
            j <- 0 until retained.length if j != i
            r <- retained(j).toCaptureRefs
          yield r
        val remaining = CaptureSet(others*)
        check(remaining, remaining)
      end for
    end for
  end checkWellformed

  /** Check well formed at post check time. We need to wait until after
   *  recheck because we find out only then whether capture sets are empty or
   *  capture references are redundant.
   */
  private def checkWellformedLater(parent: Type, ann: Tree, tpt: Tree)(using Context): Unit =
    if !tpt.span.isZeroExtent && enclosingInlineds.isEmpty then
      todoAtPostCheck += (ctx1 =>
        checkWellformed(parent, ann, tpt)(using ctx1.withOwner(ctx.owner)))

  /** Run all well formedness tests that were deferred to post check */
  def postCheck()(using Context): Unit =
    for chk <- todoAtPostCheck do chk(ctx)
    todoAtPostCheck.clear()
end Setup
