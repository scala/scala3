package dotty.tools
package dotc
package cc

import core._
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import config.Printers.capt
import ast.tpd
import transform.Recheck.*
import CaptureSet.IdentityCaptRefMap
import Synthetics.isExcluded
import util.Property
import dotty.tools.dotc.core.Annotations.Annotation

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
class Setup(
  preRecheckPhase: DenotTransformer,
  thisPhase: DenotTransformer,
  recheckDef: (tpd.ValOrDefDef, Symbol) => Context ?=> Unit)
extends tpd.TreeTraverser:
  import tpd.*

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
  private def mapInferred(using Context) = new TypeMap:

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
          if !defn.isFunctionClass(cls) && !cls.is(JavaDefined) =>
            // We assume that Java classes can refer to capturing Scala types only indirectly,
            // using type parameters. Hence, no need to refine them.
            cls.paramGetters.foldLeft(tp) { (core, getter) =>
              if getter.termRef.isTracked then
                val getterType = tp.memberInfo(getter).strippedDealias
                RefinedType(core, getter.name,
                    CapturingType(getterType, CaptureSet.RefiningVar(ctx.owner, getter)))
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
      try ts.mapConserve(this) finally isTopLevel = saved

    def apply(t: Type) =
      val tp = expandThrowsAlias(t)
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
                  .showing(i"add function refinement $tp ($tycon1, $args1, $res1) (${tp.dealias}) --> $result", capt)
              else if (tycon1 eq tycon) && (args1 eq args0) && (res1 eq res0) then
                tp
              else
                tp.derivedAppliedType(tycon1, args1 :+ res1)
          else
            tp.derivedAppliedType(tycon1, args.mapConserve(arg => this(arg)))
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
          tp.derivedLambdaType(
            paramInfos = tp.paramInfos.mapConserve(cleanup(_).bounds),
            resType = this(tp.resType))
        case _ =>
          mapOver(tp)
      addVar(addCaptureRefinements(tp1), ctx.owner)
    end apply
  end mapInferred

  private def transformInferredType(tp: Type, boxed: Boolean)(using Context): Type =
    val tp1 = mapInferred(tp)
    if boxed then box(tp1) else tp1

  /** Recognizer for `res $throws exc`, returning `(res, exc)` in case of success */
  object throwsAlias:
    def unapply(tp: Type)(using Context): Option[(Type, Type)] = tp match
      case AppliedType(tycon, res :: exc :: Nil) if tycon.typeSymbol == defn.throwsAlias =>
        Some((res, exc))
      case _ =>
        None

  /** Expand $throws aliases. This is hard-coded here since $throws aliases in stdlib
    * are defined with `?=>` rather than `?->`.
    * We also have to add a capture set to the last expanded throws alias. I.e.
    *       T $throws E1 $throws E2
    * expands to
    *       (erased x$0: CanThrow[E1]) ?-> (erased x$1: CanThrow[E1]) ?->{x$0} T
    */
  private def expandThrowsAlias(tp: Type, encl: List[MethodType] = Nil)(using Context): Type = tp match
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
  private def expandCapabilityClass(tp: Type)(using Context): Type = tp match
    case _: TypeRef | _: AppliedType if tp.typeSymbol.hasAnnotation(defn.CapabilityAnnot) =>
      CapturingType(tp, CaptureSet.universal, boxed = false)
    case _ =>
      tp

  private def expandAliases(using Context) = new TypeMap with FollowAliases:
    def apply(t: Type) =
      val t1 = expandThrowsAlias(t)
      if t1 ne t then return this(t1)
      val t2 = expandCapabilityClass(t)
      if t2 ne t then return t2
      t match
        case t @ AnnotatedType(t1, ann) =>
          // Don't map capture sets, since that would implicitly normalize sets that
          // are not well-formed.
          t.derivedAnnotatedType(this(t1), ann)
        case _ =>
          mapOverFollowingAliases(t)

  private def transformExplicitType(tp: Type, boxed: Boolean, mapRoots: Boolean)(using Context): Type =
    val tp1 = expandAliases(if boxed then box(tp) else tp)
    val tp2 =
      if mapRoots
      then cc.mapRoots(defn.captureRoot.termRef, ctx.owner.localRoot.termRef)(tp1)
            .showing(i"map roots $tp1, ${tp1.getClass} == $result", capt)
      else tp1
    if tp2 ne tp then capt.println(i"expanded: $tp --> $tp2")
    tp2

  /** Transform type of type tree, and remember the transformed type as the type the tree */
  private def transformTT(tree: TypeTree, boxed: Boolean, exact: Boolean, mapRoots: Boolean)(using Context): Unit =
    if !tree.hasRememberedType then
      tree.rememberType(
        if tree.isInstanceOf[InferredTypeTree] && !exact
        then transformInferredType(tree.tpe, boxed)
        else transformExplicitType(tree.tpe, boxed, mapRoots))

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
    sym.updateInfoBetween(preRecheckPhase, thisPhase, info)

  def traverse(tree: Tree)(using Context): Unit =
    tree match
      case tree: DefDef =>
        if isExcluded(tree.symbol) then
          return
        inContext(ctx.withOwner(tree.symbol)):
          if tree.symbol.isAnonymousFunction && tree.symbol.definedLocalRoot.exists then
            // closures that define parameters of type caps.Root count as level owners
            tree.symbol.setNestingLevel(ctx.owner.nestingLevel + 1)
          tree.tpt match
            case tpt: TypeTree if tree.symbol.allOverriddenSymbols.hasNext =>
              tree.paramss.foreach(traverse)
              transformTT(tpt, boxed = false, exact = true, mapRoots = true)
              traverse(tree.rhs)
              //println(i"TYPE of ${tree.symbol.showLocated} = ${tpt.knownType}")
            case _ =>
              traverseChildren(tree)
      case tree @ ValDef(_, tpt: TypeTree, rhs) =>
        rhs match
          case possiblyTypedClosureDef(ddef) =>
            // toplevel closures bound to vals count as level owners
            ddef.symbol.setNestingLevel(ctx.owner.nestingLevel + 1)
          case _ =>
        transformTT(tpt,
          boxed = tree.symbol.is(Mutable),    // types of mutable variables are boxed
          exact = tree.symbol.allOverriddenSymbols.hasNext, // types of symbols that override a parent don't get a capture set
          mapRoots = true
        )
        capt.println(i"mapped $tree = ${tpt.knownType}")
        if allowUniversalInBoxed && tree.symbol.is(Mutable)
            && !tree.symbol.hasAnnotation(defn.UncheckedCapturesAnnot)
        then
          CheckCaptures.disallowRootCapabilitiesIn(tpt.knownType,
            i"Mutable variable ${tree.symbol.name}", "have type",
            "This restriction serves to prevent local capabilities from escaping the scope where they are defined.",
            tree.srcPos)
        traverse(tree.rhs)
      case tree @ TypeApply(fn, args) =>
        traverse(fn)
        for case arg: TypeTree <- args do
          transformTT(arg, boxed = true, exact = false, mapRoots = true) // type arguments in type applications are boxed

        if allowUniversalInBoxed then
          val polyType = fn.tpe.widen.asInstanceOf[TypeLambda]
          for case (arg: TypeTree, pinfo, pname) <- args.lazyZip(polyType.paramInfos).lazyZip((polyType.paramNames)) do
            if pinfo.bounds.hi.hasAnnotation(defn.Caps_SealedAnnot) then
              def where = if fn.symbol.exists then i" in an argument of ${fn.symbol}" else ""
              CheckCaptures.disallowRootCapabilitiesIn(arg.knownType,
                i"Sealed type variable $pname", "be instantiated to",
                i"This is often caused by a local capability$where\nleaking as part of its result.",
                tree.srcPos)
      case tree: Template =>
        inContext(ctx.withOwner(tree.symbol.owner)):
          traverseChildren(tree)
      case _ =>
        traverseChildren(tree)
    postProcess(tree)
  end traverse

  def postProcess(tree: Tree)(using Context): Unit = tree match
    case tree: TypeTree =>
      transformTT(tree, boxed = false, exact = false,
          mapRoots = !ctx.owner.levelOwner.isStaticOwner // other types in static locaations are not boxed
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

      // Replace an existing symbol info with inferred types where capture sets of
      // TypeParamRefs and TermParamRefs put in correspondence by BiTypeMaps with the
      // capture sets of the types of the method's parameter symbols and result type.
      def integrateRT(
          info: Type,                     // symbol info to replace
          psymss: List[List[Symbol]],     // the local (type and term) parameter symbols corresponding to `info`
          prevPsymss: List[List[Symbol]], // the local parameter symbols seen previously in reverse order
          prevLambdas: List[LambdaType]   // the outer method and polytypes generated previously in reverse order
        ): Type =
        info match
          case mt: MethodOrPoly =>
            val psyms = psymss.head
            val mapr =
              if sym.isAnonymousFunction then identity[Type]
              else mapRoots(sym.localRoot.termRef, defn.captureRoot.termRef)
            mt.companion(mt.paramNames)(
              mt1 =>
                if !psyms.exists(_.isUpdatedAfter(preRecheckPhase)) && !mt.isParamDependent && prevLambdas.isEmpty then
                  mt.paramInfos
                else
                  val subst = SubstParams(psyms :: prevPsymss, mt1 :: prevLambdas)
                  psyms.map(psym => mapr(subst(psym.info)).asInstanceOf[mt.PInfo]),
              mt1 =>
                integrateRT(mapr(mt.resType), psymss.tail, psyms :: prevPsymss, mt1 :: prevLambdas)
            )
          case info: ExprType =>
            info.derivedExprType(resType =
              integrateRT(info.resType, psymss, prevPsymss, prevLambdas))
          case info =>
            if prevLambdas.isEmpty then localReturnType
            else SubstParams(prevPsymss, prevLambdas)(localReturnType)

      def signatureChanges =
        tree.tpt.hasRememberedType && !sym.isConstructor
        || tree.match
          case tree: DefDef => tree.termParamss.nestedExists(_.tpt.hasRememberedType)
          case _ => false

      if sym.exists && signatureChanges then
        val newInfo = integrateRT(sym.info, sym.paramSymss, Nil, Nil)
          .showing(i"update info $sym: ${sym.info} --> $result", capt)
        if newInfo ne sym.info then
          updateInfo(sym,
            if sym.isAnonymousFunction then
              // closures are handled specially; the newInfo is constrained from
              // the expected type and only afterwards we recheck the definition
              newInfo
            else new LazyType:
              def complete(denot: SymDenotation)(using Context) =
                // infos other methods are determined from their definitions which
                // are checked on depand
                denot.info = newInfo
                recheckDef(tree, sym))
    case tree: Bind =>
      val sym = tree.symbol
      updateInfo(sym, transformInferredType(sym.info, boxed = false))
    case tree: TypeDef =>
      tree.symbol match
        case cls: ClassSymbol =>
          val cinfo @ ClassInfo(prefix, _, ps, decls, selfInfo) = cls.classInfo
          if (selfInfo eq NoType) || cls.is(ModuleClass) && !cls.isStatic then
            // add capture set to self type of nested classes if no self type is given explicitly
            val selfRefs = CaptureSet.Var(cls)
              // it's unclear what the right level owner should be. A self type should
              // be able to mention class parameters, which are owned by the class; that's
              // why the class was picked as level owner. But self types should not be able
              // to mention other fields.
            val newInfo = ClassInfo(prefix, cls, ps, decls,
              CapturingType(cinfo.selfType, selfRefs)
                .showing(i"inferred self type for $cls: $result", capt))
            updateInfo(cls, newInfo)
            cls.thisType.asInstanceOf[ThisType].invalidateCaches()
            if cls.is(ModuleClass) then
              // if it's a module, the capture set of the module reference is the capture set of the self type
              val modul = cls.sourceModule
              updateInfo(modul, CapturingType(modul.info, selfRefs))
              modul.termRef.invalidateCaches()
        case _ =>
          val info = atPhase(preRecheckPhase)(tree.symbol.info)
          val newInfo = transformExplicitType(info, boxed = false, mapRoots = !ctx.owner.isStaticOwner)
          if newInfo ne info then
            updateInfo(tree.symbol, newInfo)
            capt.println(i"update info of ${tree.symbol} from $info to $newInfo")
    case _ =>
  end postProcess

  private def superTypeIsImpure(tp: Type)(using Context): Boolean = {
    tp.dealias match
      case CapturingType(_, refs) =>
        !refs.isAlwaysEmpty
      case tp: (TypeRef | AppliedType) =>
        val sym = tp.typeSymbol
        if sym.isClass then
          sym == defn.AnyClass
            // we assume Any is a shorthand of {cap} Any, so if Any is an upper
            // bound, the type is taken to be impure.
        else superTypeIsImpure(tp.superType)
      case tp: (RefinedOrRecType | MatchType) =>
        superTypeIsImpure(tp.underlying)
      case tp: AndType =>
        superTypeIsImpure(tp.tp1) || needsVariable(tp.tp2)
      case tp: OrType =>
        superTypeIsImpure(tp.tp1) && superTypeIsImpure(tp.tp2)
      case _ =>
        false
  }.showing(i"super type is impure $tp = $result", capt)

  /** Should a capture set variable be added on type `tp`? */
  def needsVariable(tp: Type)(using Context): Boolean = {
    tp.typeParams.isEmpty && tp.match
      case tp: (TypeRef | AppliedType) =>
        val sym = tp.typeSymbol
        if sym.isClass then
          !sym.isPureClass && sym != defn.AnyClass
        else
          val tp1 = tp.dealias
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

  /** Add a capture set variable to `tp` if necessary, or maybe pull out
   *  an embedded capture set variable from a part of `tp`.
   */
  def decorate(tp: Type, addedSet: Type => CaptureSet)(using Context): Type = tp match
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
      assert(refs1.elems.isEmpty)
      assert(refs2.elems.isEmpty)
      assert(tp1.isBoxed == tp2.isBoxed)
      CapturingType(AndType(parent1, parent2), refs1 ** refs2, tp1.isBoxed)
    case tp @ OrType(tp1 @ CapturingType(parent1, refs1), tp2 @ CapturingType(parent2, refs2)) =>
      assert(refs1.elems.isEmpty)
      assert(refs2.elems.isEmpty)
      assert(tp1.isBoxed == tp2.isBoxed)
      CapturingType(OrType(parent1, parent2, tp.isSoft), refs1 ++ refs2, tp1.isBoxed)
    case tp @ OrType(tp1 @ CapturingType(parent1, refs1), tp2) =>
      CapturingType(OrType(parent1, tp2, tp.isSoft), refs1, tp1.isBoxed)
    case tp @ OrType(tp1, tp2 @ CapturingType(parent2, refs2)) =>
      CapturingType(OrType(tp1, parent2, tp.isSoft), refs2, tp2.isBoxed)
    case tp: LazyRef =>
      decorate(tp.ref, addedSet)
    case _ if tp.typeSymbol == defn.FromJavaObjectSymbol =>
      // For capture checking, we assume Object from Java is the same as Any
      tp
    case _ =>
      def maybeAdd(target: Type, fallback: Type) =
        if needsVariable(target) then CapturingType(target, addedSet(target))
        else fallback
      val tp1 = tp.dealiasKeepAnnots
      if tp1 ne tp then
        val tp2 = transformExplicitType(tp1, boxed = false, mapRoots = true)
        maybeAdd(tp2, if tp2 ne tp1 then tp2 else tp)
      else maybeAdd(tp, tp)

  /** Add a capture set variable to `tp` if necessary, or maybe pull out
   *  an embedded capture set variable from a part of `tp`.
   */
  def addVar(tp: Type, owner: Symbol)(using Context): Type =
    decorate(tp,
      addedSet = _.dealias.match
        case CapturingType(_, refs) => CaptureSet.Var(owner, refs.elems)
        case _ => CaptureSet.Var(owner))

  def apply(tree: Tree)(using Context): Unit =
    traverse(tree)(using ctx.withProperty(Setup.IsDuringSetupKey, Some(())))

object Setup:
  val IsDuringSetupKey = new Property.Key[Unit]

  def isDuringSetup(using Context): Boolean =
    ctx.property(IsDuringSetupKey).isDefined
end Setup