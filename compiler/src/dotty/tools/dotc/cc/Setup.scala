package dotty.tools
package dotc
package cc

import core._
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import Annotations.Annotation
import config.Feature
import config.Printers.{capt, ccSetup}
import ast.tpd, tpd.*
import transform.{PreRecheck, Recheck}, Recheck.*
import CaptureSet.IdentityCaptRefMap
import Synthetics.isExcluded
import util.Property
import printing.{Printer, Texts}, Texts.{Text, Str}
import collection.mutable

/** Operations accessed from CheckCaptures */
trait SetupAPI:
  type DefRecheck = (tpd.ValOrDefDef, Symbol) => Context ?=> Type
  def setupUnit(tree: Tree, recheckDef: DefRecheck)(using Context): Unit
  def decorate(tp: Type, rootTarget: Symbol, addedSet: Type => CaptureSet)(using Context): Type

object Setup:
  def newScheme(using Context) = ctx.settings.YccNew.value // if new impl is conditional

  private val IsDuringSetupKey = new Property.Key[Unit]

  def isDuringSetup(using Context): Boolean =
    ctx.property(IsDuringSetupKey).isDefined
    //|| ctx.phase == Phases.checkCapturesPhase.prev

  case class Box(t: Type) extends UncachedGroundType, TermType:
    override def fallbackToText(printer: Printer): Text =
      Str("Box(") ~ printer.toText(t) ~ ")"
    def derivedBox(t1: Type): Type =
      if t1 eq t then this else Box(t1)

  trait DepFunSpecializedTypeMap(using Context) extends TypeMap:
    override def mapOver(tp: Type) = tp match
      case defn.RefinedFunctionOf(rinfo: MethodOrPoly) =>
        val rinfo1 = this(rinfo)
        if rinfo1 ne rinfo then rinfo1.toFunctionType(alwaysDependent = true)
        else tp
      case _ =>
        super.mapOver(tp)

  /** Recognizer for `res $throws exc`, returning `(res, exc)` in case of success */
  object throwsAlias:
    def unapply(tp: Type)(using Context): Option[(Type, Type)] = tp match
      case AppliedType(tycon, res :: exc :: Nil) if tycon.typeSymbol == defn.throwsAlias =>
        Some((res, exc))
      case _ =>
        None
end Setup
import Setup.*

/** A tree traverser that prepares a compilation unit to be capture checked.
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

  override def isRunnable(using Context) =
    super.isRunnable && Feature.ccEnabledSomewhere

  private val toBeUpdated = new mutable.HashSet[Symbol]

  private def newFlagsFor(symd: SymDenotation)(using Context): FlagSet =
    if symd.isAllOf(PrivateParamAccessor) && symd.owner.is(CaptureChecked) && !symd.hasAnnotation(defn.ConstructorOnlyAnnot)
    then symd.flags &~ Private | Recheck.ResetPrivate
    else symd.flags

  /**  - Reset `private` flags of parameter accessors so that we can refine them
   *     in Setup if they have non-empty capture sets.
   *   - Special handling of some symbols defined for case classes.
   *  Enabled only until recheck is finished, and provided some compilation unit
   *  is CC-enabled.
   */
  def transformSym(symd: SymDenotation)(using Context): SymDenotation =
    def needsInfoTransform = newScheme || symd.isGetter
    if !pastRecheck && Feature.ccEnabledSomewhere then
      val sym = symd.symbol
      val needsInfoTransform = sym.isDefinedInCurrentRun && !toBeUpdated.contains(sym)
      // if sym is class && level owner: add a capture root
      // translate cap/capIn to local roots
      // create local roots if necessary
      if Synthetics.needsTransform(symd) then
        Synthetics.transform(symd)
      else if symd.owner.isTerm || symd.is(CaptureChecked) || symd.owner.is(CaptureChecked) then
        val newFlags = newFlagsFor(symd)
        val newInfo =
          if needsInfoTransform then
            atPhase(thisPhase.next)(symd.maybeOwner.info) // ensure owner is completed
            transformExplicitType(sym.info, rootTarget = if newScheme then sym else NoSymbol)
          else sym.info

        if newFlags != symd.flags || (newInfo ne sym.info)
        then symd.copySymDenotation(initFlags = newFlags, info = newInfo)
        else symd
      else symd
    else symd
  end transformSym

  /** Create dependent function with underlying function class `tycon` and given
   *  arguments `argTypes` and result `resType`.
   */
  private def depFun(tycon: Type, argTypes: List[Type], resType: Type)(using Context): Type =
    MethodType.companion(
        isContextual = defn.isContextFunctionClass(tycon.classSymbol),
      )(argTypes, resType)
      .toFunctionType(alwaysDependent = true)

  /** If `tp` is an unboxed capturing type or a function returning an unboxed capturing type,
   *  convert it to be boxed.
   */
  private def box(tp: Type)(using Context): Type =
    def recur(tp: Type): Type = tp.dealias match
      case tp @ CapturingType(parent, refs) if !tp.isBoxed =>
        tp.boxed
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

  /** Perform the following transformation steps everywhere in a type:
    *  1. Drop retains annotations
    *  2. Turn plain function types into dependent function types, so that
    *     we can refer to their parameters in capture sets. Currently this is
    *     only done at the toplevel, i.e. for function types that are not
    *     themselves argument types of other function types. Without this restriction
    *     pos.../lists.scala and pos/...curried-shorthands.scala fail.
    *     Need to figure out why.
    *  3. Refine other class types C by adding capture set variables to their parameter getters
    *     (see addCaptureRefinements)
    *  4. Add capture set variables to all types that can be tracked
    *
    *  Polytype bounds are only cleaned using step 1, but not otherwise transformed.
    */
  private def mapInferred(rootTarget: Symbol)(using Context) = new TypeMap:
    override def toString = "map inferred"

    /** Drop @retains annotations everywhere */
    object cleanup extends TypeMap:
      def apply(t: Type) = t match
        case AnnotatedType(parent, annot) if annot.symbol == defn.RetainsAnnot =>
          apply(parent)
        case _ =>
          mapOver(t)

    /** Refine a possibly applied class type C where the class has tracked parameters
     *  x_1: T_1, ..., x_n: T_n to C { val x_1: CV_1 T_1, ..., val x_n: CV_n T_n }
     *  where CV_1, ..., CV_n are fresh capture sets.
     */
    def addCaptureRefinements(tp: Type): Type = tp match
      case _: TypeRef | _: AppliedType if tp.typeParams.isEmpty =>
        tp.typeSymbol match
          case cls: ClassSymbol
          if !defn.isFunctionClass(cls) && cls.is(CaptureChecked) =>
            cls.paramGetters.foldLeft(tp) { (core, getter) =>
              if atPhase(thisPhase.next)(getter.termRef.isTracked) then
                val getterType = tp.memberInfo(getter).strippedDealias
                RefinedType(core, getter.name,
                    CapturingType(getterType, CaptureSet.RefiningVar(ctx.owner, getter)))
                  .showing(i"add capture refinement $tp --> $result", ccSetup)
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
        case AnnotatedType(parent, annot) if annot.symbol == defn.RetainsAnnot =>
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
                depFun(tycon1, args1, res1)
                  .showing(i"add function refinement $tp ($tycon1, $args1, $res1) (${tp.dealias}) --> $result", ccSetup)
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
        case tp: MethodType =>
          tp.derivedLambdaType(
            paramInfos = mapNested(tp.paramInfos),
            resType = this(tp.resType))
        case tp: TypeLambda =>
          // Don't recurse into parameter bounds, just cleanup any stray retains annotations
          // !!! TODO we should also map roots to rootvars here
          tp.derivedLambdaType(
            paramInfos = tp.paramInfos.mapConserve(cleanup(_).bounds),
            resType = this(tp.resType))
        case Box(tp1) =>
          box(this(tp1))
        case _ =>
          mapOver(tp)
      addVar(addCaptureRefinements(normalizeCaptures(tp1)), ctx.owner, rootTarget)
    end apply
  end mapInferred

  private def transformInferredType(tp: Type, rootTarget: Symbol)(using Context): Type =
    mapInferred(rootTarget)(tp)

  private def transformExplicitType(tp: Type, rootTarget: Symbol)(using Context): Type =
    val expandAliases = new TypeMap:
      override def toString = "expand aliases"

      /** Expand $throws aliases. This is hard-coded here since $throws aliases in stdlib
        * are defined with `?=>` rather than `?->`.
        * We also have to add a capture set to the last expanded throws alias. I.e.
        *       T $throws E1 $throws E2
        * expands to
        *       (erased x$0: CanThrow[E1]) ?-> (erased x$1: CanThrow[E1]) ?->{x$0} T
        */
      private def expandThrowsAlias(tp: Type, encl: List[MethodType] = Nil): Type = tp match
        case throwsAlias(res, exc) =>
          val paramType = AnnotatedType(
              defn.CanThrowClass.typeRef.appliedTo(exc),
              Annotation(defn.ErasedParamAnnot, defn.CanThrowClass.span))
          val isLast = throwsAlias.unapply(res).isEmpty
          val paramName = nme.syntheticParamName(encl.length)
          val mt = ContextualMethodType(paramName :: Nil)(
            _ => paramType :: Nil,
            mt => if isLast then res else expandThrowsAlias(res, mt :: encl))
          val fntpe = defn.PolyFunctionOf(mt)
          if !encl.isEmpty && isLast then
            val cs = CaptureSet(encl.map(_.paramRefs.head)*)
            CapturingType(fntpe, cs, boxed = false)
          else fntpe
        case _ => tp

      /** Map references to capability classes C to C^ */
      private def expandCapabilityClass(tp: Type): Type =
        if tp.isCapabilityClassRef
        then CapturingType(tp, defn.expandedUniversalSet, boxed = false)
        else tp

      private def checkQualifiedRoots(tree: Tree): Unit =
        for case elem @ QualifiedRoot(outer) <- tree.retainedElems do
          if !ctx.owner.levelOwnerNamed(outer).exists then
            report.error(em"`$outer` does not name an outer definition that represents a capture level", elem.srcPos)

      private def recur(t: Type): Type = normalizeCaptures(mapOver(t))

      def apply(t: Type) =
        val t1 = expandThrowsAlias(t)
        if t1 ne t then return this(t1)
        val t2 = expandCapabilityClass(t)
        if t2 ne t then return t2
        t match
          case t: TypeRef =>
            val sym = t.symbol
            if sym.isClass then t
            else t.info match
              case TypeAlias(alias) =>
                val transformed =
                  if sym.isStatic then this(alias)
                  else transformExplicitType(alias,
                    if rootTarget.exists then sym else NoSymbol)(
                    using ctx.withOwner(sym.owner))
                if transformed ne alias then transformed else t
                    //.showing(i"EXPAND $t with ${t.info} to $result in ${t.symbol.owner}/${ctx.owner}")
              case _ =>
                recur(t)
          case t @ AppliedType(tycon: TypeProxy, args) if true =>
            tycon.underlying match
              case TypeAlias(aliasTycon) =>
                val args1 =
                  if defn.isFunctionClass(t.dealias.typeSymbol) then args
                  else args.map(Box(_))
                val alias = aliasTycon.applyIfParameterized(args1)
                val transformed = this(alias)
                if transformed ne alias then
                  //println(i"APP ALIAS $t / $alias / $transformed")
                  transformed
                else
                  //println(i"NO ALIAS $t / $alias / $transformed")
                  recur(t)
              case _ =>
                recur(t)
          case t @ CapturingType(parent, refs) =>
            checkQualifiedRoots(t.annot.tree) // TODO: NEEDED?
            t.derivedCapturingType(this(parent), refs)
          case t @ AnnotatedType(parent, ann) =>
            if ann.symbol == defn.RetainsAnnot then
              checkQualifiedRoots(ann.tree)
              CapturingType(this(parent), ann.tree.toCaptureSet)
            else
              t.derivedAnnotatedType(this(parent), ann)
          case Box(t1) =>
            box(this(t1))
          case t: LazyRef =>
            val t1 = this(t.ref)
            if t1 ne t.ref then t1 else t
          case t: TypeVar =>
            this(t.underlying)
          case _ =>
            recur(t)
    end expandAliases

    val tp1 = expandAliases(tp)
    val tp2 =
      if rootTarget.exists
      then mapRoots(defn.captureRoot.termRef, rootTarget.localRoot.termRef)(tp1)
            .showing(i"map roots $tp1, ${tp1.getClass} == $result", capt)
      else tp1
    if tp2 ne tp then ccSetup.println(i"expanded in ${ctx.owner}: $tp  -->  $tp1  -->  $tp2")
    tp2
  end transformExplicitType

  /** Transform type of type tree, and remember the transformed type as the type the tree */
  private def transformTT(tree: TypeTree, boxed: Boolean, exact: Boolean, rootTarget: Symbol)(using Context): Unit =
    if !tree.hasRememberedType then
      val tp = if boxed then Box(tree.tpe) else tree.tpe
      tree.rememberType(
        if tree.isInstanceOf[InferredTypeTree] && !exact
        then transformInferredType(tp, rootTarget)
        else transformExplicitType(tp, rootTarget))

  /** Substitute parameter symbols in `from` to paramRefs in corresponding
   *  method or poly types `to`. We use a single BiTypeMap to do everything.
   *  @param from  a list of lists of type or term parameter symbols of a curried method
   *  @param to    a list of method or poly types corresponding one-to-one to the parameter lists
   */
  private class SubstParams(from: List[List[Symbol]], to: List[LambdaType])(using Context)
  extends DeepTypeMap, BiTypeMap:
    thisMap =>

    def apply(t: Type): Type = t match
      case t: NamedType =>
        val sym = t.symbol
        def outer(froms: List[List[Symbol]], tos: List[LambdaType]): Type =
          def inner(from: List[Symbol], to: List[ParamRef]): Type =
            if from.isEmpty then outer(froms.tail, tos.tail)
            else if sym eq from.head then to.head
            else inner(from.tail, to.tail)
          if tos.isEmpty then t
          else inner(froms.head, tos.head.paramRefs)
        outer(from, to)
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
      def inverse = thisMap
  end SubstParams

  /** Update info of `sym` for CheckCaptures phase only */
  private def updateInfo(sym: Symbol, info: Type)(using Context) =
    toBeUpdated += sym
    sym.updateInfo(thisPhase, info, newFlagsFor(sym))
    toBeUpdated -= sym
    sym.namedType match
      case ref: CaptureRef => ref.invalidateCaches() // TODO: needed?
      case _ =>

  extension (sym: Symbol) def nextInfo(using Context): Type =
    atPhase(thisPhase.next)(sym.info)

  def setupTraverser(recheckDef: DefRecheck) = new TreeTraverserWithPreciseImportContexts:

    def traverse(tree: Tree)(using Context): Unit =
      tree match
        case tree @ DefDef(_, paramss, tpt: TypeTree, _) =>
          val meth = tree.symbol
          if isExcluded(meth) then
            return

          inContext(ctx.withOwner(meth)):
            paramss.foreach(traverse)
            transformTT(tpt, boxed = false,
                exact = tree.symbol.allOverriddenSymbols.hasNext,
                rootTarget = ctx.owner)
            traverse(tree.rhs)
            //println(i"TYPE of ${tree.symbol.showLocated} = ${tpt.knownType}")

        case tree @ ValDef(_, tpt: TypeTree, _) =>
          def containsCap(tp: Type) = tp.existsPart:
            case CapturingType(_, refs) => refs.isUniversal
            case _ => false
          def mentionsCap(tree: Tree): Boolean = tree match
            case Apply(fn, _) => mentionsCap(fn)
            case TypeApply(fn, args) => args.exists(mentionsCap)
            case _: InferredTypeTree => false
            case _: TypeTree => containsCap(transformExplicitType(tree.tpe, rootTarget = NoSymbol))
            case _ => false

          val sym = tree.symbol
          val defCtx = if sym.isOneOf(TermParamOrAccessor) then ctx else ctx.withOwner(sym)
          inContext(defCtx):
            val rootsNeedMap =
              // need to run before cc so that rhsClosure is defined without transforming
              // symbols to cc, since rhsClosure is used in that transformation.
              tree.rhs match
                case possiblyTypedClosureDef(ddef) if !mentionsCap(rhsOfEtaExpansion(ddef)) =>
                  //ddef.symbol.setNestingLevel(ctx.owner.nestingLevel + 1)
                  if newScheme then ccState.isLevelOwner(sym) = true
                  ccState.isLevelOwner(ddef.symbol) = true
                    // Toplevel closures bound to vals count as level owners
                    // unless the closure is an implicit eta expansion over a type application
                    // that mentions `cap`. In that case we prefer not to silently rebind
                    // the `cap` to a local root of an invisible closure. See
                    // pos-custom-args/captures/eta-expansions.scala for examples of both cases.
                  newScheme || !tpt.isInstanceOf[InferredTypeTree]
                    // in this case roots in inferred val type count as polymorphic
                case _ =>
                  true
            transformTT(tpt,
                boxed = sym.is(Mutable),    // types of mutable variables are boxed
                exact = sym.allOverriddenSymbols.hasNext, // types of symbols that override a parent don't get a capture set
                rootTarget = if rootsNeedMap then ctx.owner else NoSymbol
              )
            ccSetup.println(i"mapped $tree = ${tpt.knownType}")
            traverse(tree.rhs)

        case tree @ TypeApply(fn, args) =>
          traverse(fn)
          for case arg: TypeTree <- args do
            transformTT(arg, boxed = true, exact = false, rootTarget = ctx.owner) // type arguments in type applications are boxed

        case tree: TypeDef if tree.symbol.isClass =>
          inContext(ctx.withOwner(tree.symbol)):
            traverseChildren(tree)

        case _ =>
          traverseChildren(tree)
      postProcess(tree)
    end traverse

    extension (sym: Symbol)(using Context) def unlessStatic =
      val owner = sym.levelOwner
      if sym.isStaticOwner then NoSymbol else owner

    def postProcess(tree: Tree)(using Context): Unit = tree match
      case tree: TypeTree =>
        val lowner =
        transformTT(tree, boxed = false, exact = false,
            rootTarget = ctx.owner.unlessStatic // other types in static locations are not boxed
          )
      case tree: ValOrDefDef =>
        val sym = tree.symbol

        /** The return type of a constructor instantiated with local type and value
         *  parameters. Constructors have `unit` result type, that's why we can't
         *  get this type by reading the result type tree, and have to construct it
         *  explicitly.
         */
        def constrReturnType(info: Type, psymss: List[List[Symbol]]): Type = info match
          case info: MethodOrPoly =>
            constrReturnType(info.instantiate(psymss.head.map(_.namedType)), psymss.tail)
          case _ =>
            info

        /** The local result type, which is the known type of the result type tree,
         *  with special treatment for constructors.
         */
        def localReturnType =
          if sym.isConstructor then constrReturnType(sym.info, sym.paramSymss)
          else tree.tpt.knownType

        def paramSignatureChanges = tree.match
          case tree: DefDef => tree.paramss.nestedExists:
            case param: ValDef => param.tpt.hasRememberedType
            case param: TypeDef => param.rhs.hasRememberedType
          case _ => false

        def signatureChanges =
          tree.tpt.hasRememberedType && !sym.isConstructor || paramSignatureChanges

        // Replace an existing symbol info with inferred types where capture sets of
        // TypeParamRefs and TermParamRefs put in correspondence by BiTypeMaps with the
        // capture sets of the types of the method's parameter symbols and result type.
        def integrateRT(
            info: Type,                     // symbol info to replace
            psymss: List[List[Symbol]],     // the local (type and term) parameter symbols corresponding to `info`
            resType: Type,                  // the locally computed return type
            prevPsymss: List[List[Symbol]], // the local parameter symbols seen previously in reverse order
            prevLambdas: List[LambdaType]   // the outer method and polytypes generated previously in reverse order
          ): Type =
          val mapr =
            if !newScheme && sym.isLevelOwner
            then mapRoots(sym.localRoot.termRef, defn.captureRoot.termRef)
            else identity[Type]
          info match
            case mt: MethodOrPoly =>
              val psyms = psymss.head
              mt.companion(mt.paramNames)(
                mt1 =>
                  if !paramSignatureChanges && !mt.isParamDependent && prevLambdas.isEmpty then
                    mt.paramInfos
                  else
                    val subst = SubstParams(psyms :: prevPsymss, mt1 :: prevLambdas)
                    psyms.map(psym => mapr(subst(psym.nextInfo)).asInstanceOf[mt.PInfo]),
                mt1 =>
                  mapr(integrateRT(mt.resType, psymss.tail, resType, psyms :: prevPsymss, mt1 :: prevLambdas))
              )
            case info: ExprType =>
              info.derivedExprType(resType =
                mapr(integrateRT(info.resType, psymss, resType, prevPsymss, prevLambdas)))
            case info =>
              mapr(
                if prevLambdas.isEmpty then resType
                else SubstParams(prevPsymss, prevLambdas)(resType))

        if sym.exists && signatureChanges then
          def absInfo(resType: Type): Type =
            integrateRT(sym.info, sym.paramSymss, resType, Nil, Nil)
            .showing(i"update info $sym: ${sym.info} = $result", ccSetup)
          val newInfo = absInfo(localReturnType)
          if newInfo ne sym.info then
            updateInfo(sym,
              if sym.isAnonymousFunction || sym.is(Param) || sym.is(ParamAccessor) then
                // closures are handled specially; the newInfo is constrained from
                // the expected type and only afterwards we recheck the definition
                newInfo
              else new LazyType:
                def complete(denot: SymDenotation)(using Context) =
                  // infos of other methods are determined from their definitions which
                  // are checked on demand
                  assert(!isDuringSetup, i"$sym")
                  assert(ctx.phase == thisPhase.next, i"$sym")
                  ccSetup.println(i"FORCING $sym")
                  denot.info = newInfo
                  recheckDef(tree, sym))

      case tree: Bind =>
        val sym = tree.symbol
        updateInfo(sym, transformInferredType(sym.info, rootTarget = ctx.owner))
      case tree: TypeDef =>
        tree.symbol match
          case cls: ClassSymbol =>
            val cinfo @ ClassInfo(prefix, _, ps, decls, selfInfo) = cls.classInfo
            val newSelfType =
              if (selfInfo eq NoType) || cls.is(ModuleClass) && !cls.isStatic then
                // add capture set to self type of nested classes if no self type is given explicitly.
                // It's unclear what the right level owner should be. A self type should
                // be able to mention class parameters, which are owned by the class; that's
                // why the class was picked as level owner. But self types should not be able
                // to mention other fields.
                CapturingType(cinfo.selfType, CaptureSet.Var(cls))
              else selfInfo match
                case selfInfo: Type =>
                  inContext(ctx.withOwner(cls)):
                    transformExplicitType(selfInfo, rootTarget = ctx.owner)
                case _ =>
                  NoType
            if newSelfType.exists then
              ccSetup.println(i"mapped self type for $cls: $newSelfType, was $selfInfo")
              val newInfo = ClassInfo(prefix, cls, ps, decls, newSelfType)
              updateInfo(cls, newInfo)
              cls.thisType.asInstanceOf[ThisType].invalidateCaches()
              if cls.is(ModuleClass) then
                // if it's a module, the capture set of the module reference is the capture set of the self type
                val modul = cls.sourceModule
                updateInfo(modul, CapturingType(modul.info, newSelfType.captureSet))
                modul.termRef.invalidateCaches()
          case _ =>
            val info = tree.symbol.info
            val newInfo = transformExplicitType(info, rootTarget = ctx.owner.unlessStatic)
            updateInfo(tree.symbol, newInfo)
            if newInfo ne info then
              ccSetup.println(i"update info of ${tree.symbol} from $info to $newInfo")
      case _ =>
    end postProcess
  end setupTraverser

  private def superTypeIsImpure(tp: Type)(using Context): Boolean = {
    tp.dealiasKeepAnnots match
      case CapturingType(_, refs) =>
        !refs.isAlwaysEmpty
      case RetainingType(parent, refs) =>
        !refs.isEmpty
      case tp: (TypeRef | AppliedType) =>
        val sym = tp.typeSymbol
        if sym.isClass then
          sym == defn.AnyClass
            // we assume Any is a shorthand of {cap} Any, so if Any is an upper
            // bound, the type is taken to be impure.
        else
          sym != defn.Caps_Cap && superTypeIsImpure(tp.superType)
      case tp: (RefinedOrRecType | MatchType) =>
        superTypeIsImpure(tp.underlying)
      case tp: AndType =>
        superTypeIsImpure(tp.tp1) || needsVariable(tp.tp2)
      case tp: OrType =>
        superTypeIsImpure(tp.tp1) && superTypeIsImpure(tp.tp2)
      case _ =>
        false
  }.showing(i"super type is impure $tp = $result", ccSetup)

  /** Should a capture set variable be added on type `tp`? */
  def needsVariable(tp: Type)(using Context): Boolean = {
    tp.typeParams.isEmpty && tp.match
      case tp: (TypeRef | AppliedType) =>
        val sym = tp.typeSymbol
        if sym.isClass then
          !sym.isPureClass && sym != defn.AnyClass
        else
          val tp1 = tp.dealiasKeepAnnots
          if tp1 ne tp then needsVariable(tp1)
          else superTypeIsImpure(tp1)
      case tp: (RefinedOrRecType | MatchType) =>
        needsVariable(tp.underlying)
      case tp: AndType =>
        needsVariable(tp.tp1) && needsVariable(tp.tp2)
      case tp: OrType =>
        needsVariable(tp.tp1) || needsVariable(tp.tp2)
      case CapturingType(parent, refs) =>
        needsVariable(parent)
        && refs.isConst      // if refs is a variable, no need to add another
        && !refs.isUniversal // if refs is {cap}, an added variable would not change anything
      case AnnotatedType(parent, _) =>
        needsVariable(parent)
      case _ =>
        false
  }.showing(i"can have inferred capture $tp = $result", capt)

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
    case tp @ AppliedType(tycon, args) if !defn.isFunctionClass(tp.dealias.typeSymbol) =>
      tp.derivedAppliedType(tycon, args.mapConserve(box))
    case tp: LazyRef =>
      normalizeCaptures(tp.ref)
    case _ =>
      tp

  /** Add a capture set variable to `tp` if necessary, or maybe pull out
   *  an embedded capture set variable from a part of `tp`.
   */
  def decorate(tp: Type, rootTarget: Symbol, addedSet: Type => CaptureSet)(using Context): Type =
    if tp.typeSymbol == defn.FromJavaObjectSymbol then
      // For capture checking, we assume Object from Java is the same as Any
      tp
    else
      def maybeAdd(target: Type, fallback: Type) =
        if needsVariable(target) then CapturingType(target, addedSet(target))
        else fallback
      val dealiased = tp.dealiasKeepAnnots
      if dealiased ne tp then
        val transformed = transformExplicitType(dealiased, rootTarget)
          // !!! TODO: We should map roots to root vars here
        maybeAdd(transformed, if transformed ne dealiased then transformed else tp)
      else maybeAdd(tp, tp)

  /** Add a capture set variable to `tp` if necessary, or maybe pull out
   *  an embedded capture set variable from a part of `tp`.
   */
  def addVar(tp: Type, owner: Symbol, rootTarget: Symbol)(using Context): Type =
    decorate(tp, rootTarget,
      addedSet = _.dealias.match
        case CapturingType(_, refs) => CaptureSet.Var(owner, refs.elems)
        case _ => CaptureSet.Var(owner))

  def setupUnit(tree: Tree, recheckDef: DefRecheck)(using Context): Unit =
    setupTraverser(recheckDef).traverse(tree)(
      using ctx.withPhase(thisPhase).withProperty(IsDuringSetupKey, Some(())))

end Setup