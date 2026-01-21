package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import Names.{Name, TermName}
import ast.{tpd, untpd}
import Decorators.*, NameOps.*
import config.Printers.capt
import util.Property.Key
import tpd.*
import Annotations.Annotation
import CaptureSet.VarState
import Capabilities.*
import Mutability.isStatefulType
import StdNames.{nme, tpnme}
import config.Feature
import NameKinds.TryOwnerName
import typer.ProtoTypes.WildcardSelectionProto

/** Are we at checkCaptures phase? */
def isCaptureChecking(using Context): Boolean =
  ctx.phaseId == Phases.checkCapturesPhaseId

/** Are we in the CheckCaptures or Setup phase? */
def isCaptureCheckingOrSetup(using Context): Boolean =
  val ccId = Phases.checkCapturesPhaseId
  val ctxId = ctx.phaseId
  ctxId == ccId
  || ctxId == ccId - 1 && ccState.iterationId > 0
    // Note: just checking phase id is not enough since Setup would
    // also be the phase after pattern matcher.

/** A dependent function type with given arguments and result type
 *  TODO Move somewhere else where we treat all function type related ops together.
 */
def depFun(args: List[Type], resultType: Type, isContextual: Boolean, paramNames: List[TermName] = Nil)(using Context): Type =
  val make = MethodType.companion(isContextual = isContextual)
  val mt =
    if paramNames.length == args.length then make(paramNames, args, resultType)
    else make(args, resultType)
  mt.toFunctionType(alwaysDependent = true)

/** An exception thrown if a @retains argument is not syntactically a Capability */
class IllegalCaptureRef(tpe: Type)(using Context) extends Exception(tpe.show)

/** The currently valid CCState */
def ccState(using Context): CCState =
  Phases.checkCapturesPhase.asInstanceOf[CheckCaptures].ccState1

extension (tree: Tree)

  /** The type representing the capture set of @retains, @retainsCap or @retainsByName
   *  annotation tree (represented as an Apply node).
   */
  def retainedSet(using Context): Type = tree match
    case Apply(TypeApply(_, refs :: Nil), _) => refs.tpe
    case _ =>
      if tree.symbol.maybeOwner == defn.RetainsCapAnnot
      then defn.Caps_any.termRef
      else NoType

extension (tp: Type)

  def toCapability(using Context): Capability = tp match
    case ReachCapability(tp1) =>
      tp1.toCapability.reach
    case ReadOnlyCapability(tp1) =>
      tp1.toCapability.readOnly
    case OnlyCapability(tp1, cls) =>
      tp1.toCapability.restrict(cls)
    case ref: TermRef if ref.isCapsAnyRef =>
      GlobalAny
    case ref: TermRef if ref.isCapsFreshRef =>
      GlobalFresh
    case ref: Capability if ref.isTrackableRef =>
      ref
    case ref: TermRef if ref.isLocalMutable =>
      ref.mapLocalMutable
    case _ =>
      // if this was compiled from cc syntax, problem should have been reported at Typer
      throw IllegalCaptureRef(tp)

  /** A list of raw elements of a retained set.
   *  This will not crash even if it contains a non-wellformed Capability.
   */
  def retainedElementsRaw(using Context): List[Type] = tp match
    case OrType(tp1, tp2) =>
      tp1.retainedElementsRaw ++ tp2.retainedElementsRaw
    case AnnotatedType(tp1, ann: RetainingAnnotation) if tp1.derivesFrom(defn.Caps_CapSet) =>
      ann.retainedType.retainedElementsRaw
    case tp =>
      tp.dealiasKeepAnnots match
        case tp: TypeRef if tp.symbol == defn.Caps_CapSet =>
          // This can happen in cases where we try to type an eta expansion `$x => f($x)`
          // from a polymorphic target type using capture sets. In that case the parameter type
          // of $x is not treated as inferred and is approximated to CapSet. An example is
          // capset-problem.scala. We handle these cases by appromxating to the empty set.
          Nil
        case _ =>
          // Nothing is a special type to represent the empty set
          if tp.isNothingType then Nil
          else tp :: Nil // should be checked by wellformedness

  /** A list of capabilities of a retained set. */
  def retainedElements(using Context): List[Capability] =
    retainedElementsRaw.flatMap: elem =>
      elem match
        case CapturingType(parent, refs) if parent.derivesFrom(defn.Caps_CapSet) =>
          refs.elems.toList
        case _ =>
          elem.toCapability :: Nil

  /** Is this type a Capability that can be tracked?
   *  This is true for
   *    - all ThisTypes and all TermParamRef,
   *    - stable TermRefs with NoPrefix or ThisTypes as prefixes,
   *    - annotated types that represent reach or maybe capabilities
   */
  final def isTrackableRef(using Context): Boolean = tp match
    case _: (ThisType | TermParamRef) => true
    case tp: TermRef =>
      !tp.underlying.exists // might happen during construction of lambdas with annotations on parameters
      ||
        ((tp.prefix eq NoPrefix)
        || tp.symbol.isField && tp.prefix.isPrefixOfTrackableRef
        ) && !tp.symbol.isOneOf(UnstableValueFlags)
    case tp: TypeRef =>
      tp.symbol.isType && tp.derivesFrom(defn.Caps_CapSet)
    case tp: TypeParamRef =>
      !tp.underlying.exists // might happen during construction of lambdas
      || tp.derivesFrom(defn.Caps_CapSet)
    case _ =>
      false

  private def isPrefixOfTrackableRef(using Context): Boolean =
    isTrackableRef || tp.match
      case tp: TermRef => tp.symbol.is(Package)
      case _ => false

  /** The capture set of a type. This is:
    *   - For object capabilities: The singleton capture set consisting of
    *     just the reference, provided the underlying capture set of their info is not empty.
    *   - For other capabilities: The capture set of their info
    *   - For all other types: The result of CaptureSet.ofType
    */
  final def captureSet(using Context): CaptureSet = tp match
    case tp: CoreCapability if tp.isTrackableRef =>
      val cs = tp.captureSetOfInfo
      if cs.isAlwaysEmpty then cs else tp.singletonCaptureSet
    case tp: ObjectCapability => tp.captureSetOfInfo
    case _ => CaptureSet.ofType(tp, followResult = false)

  /** Compute a captureset by traversing parts of this type. This is by default the union of all
   *  covariant capture sets embedded in the widened type, as computed by
   *  `CaptureSet.ofTypeDeeply`. If that set is nonempty, and the type is
   *  a singleton capability `x` or a reach capability `x*`, the deep capture
   *  set can be narrowed to`{x*}`.
   *  @param includeTypevars  if true, return a new LocalCap for every type parameter
   *                          or abstract type with an Any upper bound. Types with
   *                          defined upper bound are always mapped to the dcs of their bound
   *  @param includeBoxed     if true, include capture sets found in boxed parts of this type
   */
  def computeDeepCaptureSet(includeTypevars: Boolean, includeBoxed: Boolean = true)(using Context): CaptureSet =
    val dcs = CaptureSet.ofTypeDeeply(tp.widen.stripCapturing, includeTypevars, includeBoxed)
    if dcs.isAlwaysEmpty then tp.captureSet
    else tp match
      case tp: ObjectCapability if tp.isTrackableRef => tp.reach.singletonCaptureSet
      case _ => tp.captureSet ++ dcs

  /** The deep capture set of a type. This is by default the union of all
   *  covariant capture sets embedded in the widened type, as computed by
   *  `CaptureSet.ofTypeDeeply`. If that set is nonempty, and the type is
   *  a singleton capability `x` or a reach capability `x*`, the deep capture
   *  set can be narrowed to`{x*}`.
   */
  def deepCaptureSet(using Context): CaptureSet =
    computeDeepCaptureSet(includeTypevars = false)

  /** The span capture set of a type. This is analogous to deepCaptureSet but ignoring
   *  capture sets in boxed parts.
   */
  def spanCaptureSet(using Context): CaptureSet =
    computeDeepCaptureSet(includeTypevars = false, includeBoxed = false)

  /** A type capturing `ref` */
  def capturing(ref: Capability)(using Context): Type =
    if tp.captureSet.accountsFor(ref) then tp
    else CapturingType(tp, ref.singletonCaptureSet)

  /** A type capturing the capture set `cs`. If this type is already a capturing type
   *  the two capture sets are combined.
   */
  def capturing(cs: CaptureSet)(using Context): Type =
    if (cs.isAlwaysEmpty || cs.isConst && cs.subCaptures(tp.captureSet, VarState.Separate))
        && !cs.keepAlways
    then tp
    else tp match
      case CapturingType(parent, cs1) => parent.capturing(cs1 ++ cs)
      case _ => CapturingType(tp, cs)

  /** @pre `tp` is a CapturingType */
  def derivedCapturingType(parent: Type, refs: CaptureSet)(using Context): Type = tp match
    case tp @ CapturingType(p, r) =>
      if (parent eq p) && (refs eq r) then tp
      else CapturingType(parent, refs, tp.isBoxed)

  /** If this is a unboxed capturing type with nonempty capture set, its boxed version.
   *  Or, if type is a TypeBounds of capturing types, the version where the bounds are boxed.
   *  The identity for all other types.
   */
  def boxed(using Context): Type = tp.dealias match
    case tp @ CapturingType(parent, refs) if !tp.isBoxed && !refs.isAlwaysEmpty =>
      tp.annot match
        case ann: CaptureAnnotation if !parent.derivesFrom(defn.Caps_CapSet) =>
          AnnotatedType(parent, ann.boxedAnnot)
        case ann => tp
    case tp: RealTypeBounds =>
      tp.derivedTypeBounds(tp.lo.boxed, tp.hi.boxed)
    case _ =>
      tp

  /** If this is a unboxed capturing type with nonempty capture set, its boxed version.
   *  Or, if type is a TypeBounds of capturing types, the version where the bounds are boxed.
   *  The identity for all other types.
   */
  def unboxed(using Context): Type = tp.dealias match
    case tp @ CapturingType(parent, refs) if tp.isBoxed && !refs.isAlwaysEmpty =>
      CapturingType(parent, refs)
    case tp: RealTypeBounds =>
      tp.derivedTypeBounds(tp.lo.unboxed, tp.hi.unboxed)
    case _ =>
      tp

  /** If `tp` is an unboxed capturing type or a function returning an unboxed capturing type,
   *  convert it to be boxed.
   */
  def boxDeeply(using Context): Type =
    def recur(tp: Type): Type = tp.dealiasKeepAnnotsAndOpaques match
      case tp @ CapturingType(parent, refs) =>
        if tp.isBoxed || parent.derivesFrom(defn.Caps_CapSet) then tp
        else tp.boxed
      case tp @ AnnotatedType(parent, ann: RetainingAnnotation)
      if !parent.derivesFrom(defn.Caps_CapSet) =>
        assert(ann.isStrict)
        CapturingType(parent, ann.toCaptureSet, boxed = true)
      case tp @ AnnotatedType(parent, ann) =>
        tp.derivedAnnotatedType(parent.boxDeeply, ann)
      case tp: (Capability & SingletonType) if tp.isTrackableRef && !tp.isAlwaysPure =>
        recur(CapturingType(tp, CaptureSet(tp)))
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

  /** The capture set consisting of all top-level captures of `tp` that appear under a box.
   *  Unlike for `boxed` this also considers parents of capture types, unions and
   *  intersections, and type proxies other than abstract types.
   *  Furthermore, if the original type is a capability `x`, it replaces boxed universal sets
   *  on the fly with x*.
   */
  def boxedCaptureSet(using Context): CaptureSet =
    def getBoxed(tp: Type, pre: Type): CaptureSet = tp match
      case tp @ CapturingType(parent, refs) =>
        val pcs = getBoxed(parent, pre)
        if !tp.isBoxed then
          pcs
        else pre match
          case pre: ObjectCapability if refs.containsTerminalCapability =>
            val reachRef = if refs.isReadOnly then pre.reach.readOnly else pre.reach
            pcs ++ reachRef.singletonCaptureSet
          case _ =>
            pcs ++ refs
      case ref: Capability if ref.isTracked && !pre.exists => getBoxed(ref, ref)
      case tp: TypeRef if tp.symbol.isAbstractOrParamType => CaptureSet.empty
      case tp: TypeProxy => getBoxed(tp.superType, pre)
      case tp: AndType => getBoxed(tp.tp1, pre) ** getBoxed(tp.tp2, pre)
      case tp: OrType => getBoxed(tp.tp1, pre) ++ getBoxed(tp.tp2, pre)
      case _ => CaptureSet.empty
    getBoxed(tp, NoType)

  /** Is the boxedCaptureSet of this type nonempty? */
  def isBoxedCapturing(using Context): Boolean =
    tp match
      case tp @ CapturingType(parent, refs) =>
        tp.isBoxed && !refs.isAlwaysEmpty || parent.isBoxedCapturing
      case tp: TypeRef if tp.symbol.isAbstractOrParamType => false
      case tp: TypeProxy => tp.superType.isBoxedCapturing
      case tp: AndType => tp.tp1.isBoxedCapturing && tp.tp2.isBoxedCapturing
      case tp: OrType => tp.tp1.isBoxedCapturing || tp.tp2.isBoxedCapturing
      case _ => false

  /** Is the box status of `tp` and `tp2` compatible? I.ee  they are
   *  box boxed, or both unboxed, or one of them has an empty capture set.
   */
  def isBoxCompatibleWith(tp2: Type)(using Context): Boolean =
    isBoxedCapturing == tp2.isBoxedCapturing
    || tp.captureSet.isAlwaysEmpty
    || tp2.captureSet.isAlwaysEmpty

  /** If this type is a capturing type, the version with boxed statues as given by `boxed`.
   *  If it is a TermRef of a capturing type, and the box status flips, widen to a capturing
   *  type that captures the TermRef.
   */
  def forceBoxStatus(boxed: Boolean)(using Context): Type = tp.widenDealias match
    case tp @ CapturingType(parent, refs) if tp.isBoxed != boxed =>
      val refs1 = tp match
        case ref: Capability if ref.isTracked || ref.isInstanceOf[DerivedCapability] =>
          ref.singletonCaptureSet
        case _ => refs
      CapturingType(parent, refs1, boxed)
    case _ =>
      tp

  /** Map capturing type to their parents. Capturing types accessible
   *  via dealising are also stripped.
   */
  def stripCapturing(using Context): Type = tp.dealiasKeepAnnots match
    case CapturingType(parent, _) =>
      parent.stripCapturing
    case atd @ AnnotatedType(parent, annot) =>
      atd.derivedAnnotatedType(parent.stripCapturing, annot)
    case _ =>
      tp

  /** Is type known to be always pure by its class structure?
   *  In that case, adding a capture set to it would not make sense.
   */
  def isAlwaysPure(using Context): Boolean = tp.dealias match
    case tp: (TypeRef | AppliedType) =>
      val sym = tp.typeSymbol
      if sym.isClass then sym.isPureClass
      else !tp.superType.isAny && tp.superType.isAlwaysPure
    case tp: TypeProxy =>
      !tp.superType.isAny && tp.superType.isAlwaysPure
    case tp: AndType =>
      tp.tp1.isAlwaysPure || tp.tp2.isAlwaysPure
    case tp: OrType =>
      tp.tp1.isAlwaysPure && tp.tp2.isAlwaysPure
    case _ =>
      false

  /** Is this a reference to caps.any? Note this is _not_ the GlobalAny capability. */
  def isCapsAnyRef(using Context): Boolean = tp match
    case tp: TermRef => tp.name == nme.any && tp.symbol == defn.Caps_any
    case _ => false

  /** Is this a reference to caps.any? Note this is _not_ the GlobalFresh capability. */
  def isCapsFreshRef(using Context): Boolean = tp match
    case tp: TermRef => tp.name == nme.fresh && tp.symbol == defn.Caps_fresh
    case _ => false

  /** Knowing that `tp` is a function type, is it an alias to a function other
   *  than `=>`?
   */
  def isAliasFun(using Context): Boolean = tp match
    case AppliedType(tycon, _) => !defn.isFunctionSymbol(tycon.typeSymbol)
    case _ => false

  /** Tests whether all CapturingType parts of the type that are traversed for
   *  dcs computation satisfy at least one of two conditions:
   *   1. They decorate classes that extend the given capability class `cls`, or
   *   2. Their capture set is constant and consists only of capabilities
   *      the derive from `cls` in the sense of `derivesFromCapTrait`.
   */
  def derivesFromCapTraitDeeply(cls: ClassSymbol)(using Context): Boolean =
    val accumulate = new DeepTypeAccumulator[Boolean]:
      def capturingCase(acc: Boolean, parent: Type, refs: CaptureSet, boxed: Boolean) =
        this(acc, parent)
        && (parent.derivesFromCapTrait(cls)
            || refs.isConst && refs.elems.forall(_.derivesFromCapTrait(cls)))
      def abstractTypeCase(acc: Boolean, t: TypeRef, upperBound: Type) =
        this(acc, upperBound)
    accumulate(true, tp)

  /** Tests whether the type derives from capability class `cls`. */
  def derivesFromCapTrait(cls: ClassSymbol)(using Context): Boolean = tp.dealiasKeepAnnots match
    case tp: (TypeRef | AppliedType) =>
      val sym = tp.typeSymbol
      if sym.isClass then sym.derivesFrom(cls)
      else tp.superType.derivesFromCapTrait(cls)
    case tp: (TypeProxy & ValueType) =>
      tp.superType.derivesFromCapTrait(cls)
    case tp: AndType =>
      tp.tp1.derivesFromCapTrait(cls) || tp.tp2.derivesFromCapTrait(cls)
    case tp: OrType =>
      tp.tp1.derivesFromCapTrait(cls) && tp.tp2.derivesFromCapTrait(cls)
    case _ =>
      false

  def derivesFromCapability(using Context): Boolean =
    derivesFromCapTrait(defn.Caps_Capability) || isArrayUnderStrictMut
  def derivesFromStateful(using Context): Boolean =
    derivesFromCapTrait(defn.Caps_Stateful) || isArrayUnderStrictMut
  def derivesFromShared(using Context): Boolean =
    derivesFromCapTrait(defn.Caps_SharedCapability)
  def derivesFromUnscoped(using Context): Boolean =
    derivesFromCapTrait(defn.Caps_Unscoped) || isArrayUnderStrictMut
  def derivesFromMutable(using Context): Boolean =
    derivesFromCapTrait(defn.Caps_Mutable) || isArrayUnderStrictMut

  def isArrayUnderStrictMut(using Context): Boolean = tp.classSymbol.isArrayUnderStrictMut

  /** Drop @retains annotations everywhere */
  def dropAllRetains(using Context): Type = // TODO we should drop retains from inferred types before unpickling
    val tm = new TypeMap:
      def apply(t: Type) = t match
        case AnnotatedType(parent, annot) if annot.symbol.isRetains =>
          apply(parent)
        case _ =>
          mapOver(t)
    tm(tp)

  /** If `x` is a capability, replace all no-flip covariant occurrences of `any`
   *  in type `tp` with `x*`.
   */
  def withReachCaptures(ref: Type)(using Context): Type = ref match
    case ref: ObjectCapability if ref.isTrackableRef =>
      object narrowCaps extends TypeMap:
        var change = false
        def apply(t: Type) =
          if variance <= 0 then t
          else t.dealias match
            case t @ CapturingType(p, cs) if cs.containsGlobalOrLocalCap =>
              val reachRef = if cs.isReadOnly then ref.reach.readOnly else ref.reach
              if reachRef.singletonCaptureSet.mightSubcapture(cs) then
                change = true
                t.derivedCapturingType(apply(p), reachRef.singletonCaptureSet)
              else
                t
            case t @ AnnotatedType(parent, ann) =>
              // Don't map annotations, which includes capture sets
              t.derivedAnnotatedType(this(parent), ann)
            case t @ FunctionOrMethod(args, res) =>
              t.derivedFunctionOrMethod(args, apply(res))
            case _ =>
              mapOver(t)
      end narrowCaps
      val tp1 = narrowCaps(tp)
      if narrowCaps.change then
        capt.println(i"narrow $tp of $ref to $tp1")
        tp1
      else
        tp
    case _ =>
      tp
  end withReachCaptures

  private def containsGlobal(c: GlobalCap, directly: Boolean)(using Context): Boolean =
    val search = new TypeAccumulator[Boolean]:
      def apply(x: Boolean, t: Type) =
        if x then true
        else if variance <= 0 then false
        else if directly && defn.isFunctionSymbol(t.typeSymbol) then false
        else t match
          case CapturingType(_, refs) if refs.elems.exists(_.core == c) =>
            true
          case t @ AnnotatedType(parent, ann) =>
            // Don't traverse annotations, which includes capture sets
            this(x, parent)
          case _ =>
            foldOver(x, t)
    search(false, tp)

  /** Does this type contain no-flip covariant occurrences of `any`? */
  def containsGlobalAny(using Context): Boolean =
    containsGlobal(GlobalAny, directly = false)

  /** Does `tp` contain contain no-flip covariant occurrences of `fresh` directly,
   *  which are not in the result of some function type?
   */
  def containsGlobalFreshDirectly(using Context): Boolean =
    containsGlobal(GlobalFresh, directly = true)

  def refinedOverride(name: Name, rinfo: Type)(using Context): Type =
    RefinedType.precise(tp, name, rinfo)

  def dropUseAndConsumeAnnots(using Context): Type =
    tp.dropAnnot(defn.UseAnnot).dropAnnot(defn.ConsumeAnnot)

  /** If `tp` is a function or method, a type of the same kind with the given
   *  argument and result types.
  */
  def derivedFunctionOrMethod(argTypes: List[Type], resType: Type)(using Context): Type = tp match
    case tp @ AppliedType(tycon, args) if defn.isNonRefinedFunction(tp) =>
      val args1 = argTypes :+ resType
      if args.corresponds(args1)(_ eq _) then tp
      else tp.derivedAppliedType(tycon, args1)
    case tp @ defn.RefinedFunctionOf(rinfo) =>
      val rinfo1 = rinfo.derivedFunctionOrMethod(argTypes, resType)
      if rinfo1 eq rinfo then tp
      else if rinfo1.isInstanceOf[PolyType] then tp.derivedRefinedType(refinedInfo = rinfo1)
      else rinfo1.toFunctionType(alwaysDependent = true)
    case tp: MethodType =>
      tp.derivedLambdaType(paramInfos = argTypes, resType = resType)
    case tp: PolyType =>
      assert(argTypes.isEmpty)
      tp.derivedLambdaType(resType = resType)
    case _ =>
      tp

  def inheritedClassifier(using Context): ClassSymbol =
    if tp.isArrayUnderStrictMut then defn.Caps_Unscoped
    else tp.classSymbols.map(_.classifier).foldLeft(defn.AnyClass)(leastClassifier)

extension (tp: MethodOrPoly)
  /** A method marks an existential scope unless it is the prefix of a curried method */
  def marksExistentialScope(using Context): Boolean =
    !tp.resType.isInstanceOf[MethodOrPoly]

extension (ref: TermRef | ThisType)
  /** Map a local mutable var to its mirror */
  def mapLocalMutable(using Context): TermRef | ThisType = ref match
    case ref: TermRef if ref.isLocalMutable => ref.symbol.varMirror.termRef
    case _ => ref

extension (cls: ClassSymbol)

  def pureBaseClass(using Context): Option[Symbol] =
    cls.baseClasses.find: bc =>
      defn.pureBaseClasses.contains(bc)
      || bc.is(CaptureChecked)
          && bc.givenSelfType.dealiasKeepAnnots.match
            case CapturingOrRetainsType(_, refs) => refs.isAlwaysEmpty
            case selfType =>
              isCaptureChecking  // At Setup we have not processed self types yet, so
                                 // unless a self type is explicitly given, we can't tell
                                 // and err on the side of impure.
              && selfType.exists && selfType.captureSet.isAlwaysEmpty

  def baseClassHasExplicitNonUniversalSelfType(using Context): Boolean =
    cls.baseClasses.exists: bc =>
      bc.is(CaptureChecked)
      && bc.givenSelfType.exists
      && !bc.givenSelfType.captureSet.isUniversal

  def matchesExplicitRefsInBaseClass(refs: CaptureSet)(using Context): Boolean =
    cls.baseClasses.tail.exists: bc =>
      val selfType = bc.givenSelfType
      bc.is(CaptureChecked) && selfType.exists && selfType.captureSet.elems == refs.elems

  def isClassifiedCapabilityClass(using Context): Boolean =
    cls.derivesFrom(defn.Caps_Capability) && cls.parentSyms.contains(defn.Caps_Classifier)

  def classifier(using Context): ClassSymbol =
    if cls.derivesFrom(defn.Caps_Capability) then
      cls.baseClasses
        .filter(_.parentSyms.contains(defn.Caps_Classifier))
        .foldLeft(defn.AnyClass)(leastClassifier)
    else defn.AnyClass

  def isSeparate(using Context): Boolean =
    cls.derivesFrom(defn.Caps_Separate)
    || cls.typeRef.isStatefulType
    || cls.paramGetters.exists: getter =>
          !getter.is(Private) // Setup makes sure that getters with capture sets are not private
          && getter.hasAnnotation(defn.ConsumeAnnot)

extension (sym: Symbol)

  private def inScalaAnnotation(using Context): Boolean =
    sym.maybeOwner.name == tpnme.annotation
    && sym.owner.owner == defn.ScalaPackageClass

  /** Is this symbol one of `retains` or `retainsCap`?
   *  Try to avoid cycles by not forcing definition symbols except scala package.
   */
  def isRetains(using Context): Boolean =
    (sym.name == tpnme.retains || sym.name == tpnme.retainsCap)
    && inScalaAnnotation

  /** Is this symbol one of `retains`, `retainsCap`, or`retainsByName`?
   *  Try to avoid cycles by not forcing definition symbols except scala package.
   */
  def isRetainsLike(using Context): Boolean =
    (sym.name == tpnme.retains || sym.name == tpnme.retainsCap || sym.name == tpnme.retainsByName)
    && inScalaAnnotation

  /** A class is pure if:
   *   - one its base types has an explicitly declared self type with an empty capture set
   *   - or it is a value class
   *   - or it is an exception
   *   - or it is one of Nothing, Null, or String
   *  Arrays are not pure under strict mutability even though their self type is declared pure
   *  in Arrays.scala.
   */
  def isPureClass(using Context): Boolean = sym match
    case cls: ClassSymbol =>
      (cls.pureBaseClass.isDefined || defn.pureSimpleClasses.contains(cls))
      && !cls.isArrayUnderStrictMut
    case _ =>
      false

  /** Does this symbol allow results carrying the universal capability?
   *  Currently this is true only for function type applies (since their
   *  results are unboxed) and `caps.{$internal,unsafe}.erasedValue` since
   *  these function are magic in that they allow to conjure global capabilies from nothing.
   *  But it could be generalized to other functions that so that they can take capability
   *  classes as arguments.
   */
  def allowsRootCapture(using Context): Boolean =
    defn.capsErasedValueMethods.contains(sym) || defn.isFunctionClass(sym.maybeOwner)

  /** When applying `sym`, would the result type be unboxed?
   *  This is the case if the result type contains a top-level reference to an enclosing
   *  class or method type parameter and the method does not allow root capture.
   *  If the type parameter is instantiated to a boxed type, that type would
   *  have to be unboxed in the method's result.
   */
  def unboxesResult(using Context): Boolean =
    def containsEnclTypeParam(tp: Type): Boolean = tp.strippedDealias match
      case tp @ TypeRef(pre: ThisType, _) => tp.symbol.is(Param)
      case tp: TypeParamRef => true
      case tp: AndOrType => containsEnclTypeParam(tp.tp1) || containsEnclTypeParam(tp.tp2)
      case tp: RefinedType => containsEnclTypeParam(tp.parent) || containsEnclTypeParam(tp.refinedInfo)
      case _ => false
    containsEnclTypeParam(sym.info.finalResultType)
    && !sym.allowsRootCapture
    && !defn.isPolymorphicAfterErasure(sym)
    && !defn.isTypeTestOrCast(sym)

  /** It's a parameter accessor for a parameter that that is not annotated
   *  @constructorOnly or @uncheckedCaptures and that is not a consume parameter.
   */
  def isRefiningParamAccessor(using Context): Boolean =
    sym.is(ParamAccessor)
    && {
      val param = sym.owner.primaryConstructor.paramNamed(sym.name)
      !param.hasAnnotation(defn.ConstructorOnlyAnnot)
      && !param.hasAnnotation(defn.UntrackedCapturesAnnot)
      && !param.hasAnnotation(defn.ConsumeAnnot)
    }

  /** It's a parameter accessor that is tracked for capture checking. Excluded are
   *  accessors for parameters annotated with constructorOnly or @uncheckedCaptures.
   */
  def isTrackedParamAccessor(using Context): Boolean =
    sym.is(ParamAccessor)
    && {
      val param = sym.owner.primaryConstructor.paramNamed(sym.name)
      !param.hasAnnotation(defn.ConstructorOnlyAnnot)
      && !param.hasAnnotation(defn.UntrackedCapturesAnnot)
    }

  def hasTrackedParts(using Context): Boolean =
    !CaptureSet.ofTypeDeeply(sym.info).isAlwaysEmpty

  /** Until 3.7:
   *    `sym` itself or its info is annotated @use or it is a type parameter with a matching
   *    @use-annotated term parameter that contains `sym` in its deep capture set.
   *  From 3.8:
   *    `sym` is a capset parameter without a `@reserve` annotation that
   *      - belongs to a class in a class, or
   *      - belongs to a method where it appears in a the deep capture set of a following term parameter of the same method.
   */
  def isUseParam(using Context): Boolean =
    sym.hasAnnotation(defn.UseAnnot)
    || sym.info.hasAnnotation(defn.UseAnnot)
    || sym.is(TypeParam)
        && !sym.info.hasAnnotation(defn.ReserveAnnot)
        && (sym.owner.isClass
            || sym.owner.rawParamss.nestedExists: param =>
                param.is(TermParam)
                && (!ccConfig.allowUse || param.hasAnnotation(defn.UseAnnot))
                && param.info.deepCaptureSet.elems.exists:
                    case c: TypeRef => c.symbol == sym
                    case _ => false
            || {
              //println(i"not is use param $sym")
              false
            })

  /** `sym` or its info is annotated with `@consume`. */
  def isConsumeParam(using Context): Boolean =
    sym.hasAnnotation(defn.ConsumeAnnot)
    || sym.info.hasAnnotation(defn.ConsumeAnnot)

  def qualString(prefix: String)(using Context): String =
    if !sym.exists then "" else i" $prefix ${sym.ownerString}"

  def ownerString(using Context): String =
    if !sym.exists then ""
    else if sym.isAnonymousFunction then i"an enclosing function"
    else if sym.name.is(TryOwnerName) then i"an enclosing try expression"
    else sym.show

  def isArrayUnderStrictMut(using Context): Boolean =
    sym == defn.ArrayClass && ccConfig.strictMutability

  def isDisallowedInCapset(using Context): Boolean =
    sym.isOneOf(if ccConfig.strictMutability then Method else UnstableValueFlags)

  def varMirror(using Context): Symbol =
    ccState.varMirrors.getOrElseUpdate(sym,
      sym.copy(
        flags = Flags.EmptyFlags,
        info = defn.Caps_Var.typeRef.appliedTo(sym.info)
            .capturing(LocalCap(sym, Origin.InDecl(sym)))))

  def skipAnonymousOwners(using Context): Symbol =
    if sym.isAnonymousFunction then sym.owner.skipAnonymousOwners
    else sym

extension (tp: AnnotatedType)
  /** Is this a boxed capturing type? */
  def isBoxed(using Context): Boolean = tp.annot match
    case ann: CaptureAnnotation => ann.boxed
    case _ => false

/** A prototype that indicates selection */
class PathSelectionProto(val select: Select, val pt: Type) extends typer.ProtoTypes.WildcardSelectionProto:
  def selector(using Context): Symbol = select.symbol

/** Drop retains annotations in the inferred type if CC is not enabled
 *  or transform them into retains annotations with Nothing (i.e. empty set) as
 *   argument if CC is enabled (we need to do that to keep by-name status).
 */
class CleanupRetains(using Context) extends TypeMap:
  def apply(tp: Type): Type = tp match
    case tp @ AnnotatedType(parent, annot: RetainingAnnotation) =>
      if Feature.ccEnabled then
        if annot.symbol == defn.RetainsCapAnnot then tp
        else AnnotatedType(this(parent), RetainingAnnotation(annot.symbol.asClass, defn.NothingType))
      else this(parent)
    case _ => mapOver(tp)

/** A base class for extractors that match annotated types with a specific
 *  Capability annotation.
 */
abstract class AnnotatedCapability(annotCls: Context ?=> ClassSymbol):
  def apply(tp: Type)(using Context): AnnotatedType =
    AnnotatedType(tp, Annotation(annotCls, util.Spans.NoSpan))

  def unapply(tree: AnnotatedType)(using Context): Option[Type] = tree match
    case AnnotatedType(parent: Type, ann) if ann.hasSymbol(annotCls) => Some(parent)
    case _ => None
end AnnotatedCapability

/** An extractor for `ref @readOnlyCapability`, which is used to express
 *  the read-only capability `ref.rd` as a type.
 */
object ReadOnlyCapability extends AnnotatedCapability(defn.ReadOnlyCapabilityAnnot)

/** An extractor for `ref @reachCapability`, which is used to express
 *  the reach capability `ref*` as a type.
 */
object ReachCapability extends AnnotatedCapability(defn.ReachCapabilityAnnot)

/** An extractor for `ref @amaybeCapability`, which is used to express
 *  the maybe capability `ref?` as a type.
 */
object MaybeCapability extends AnnotatedCapability(defn.MaybeCapabilityAnnot)

object OnlyCapability:
  def apply(tp: Type, cls: ClassSymbol)(using Context): AnnotatedType =
    AnnotatedType(tp,
      Annotation(defn.OnlyCapabilityAnnot.typeRef.appliedTo(cls.typeRef), Nil, util.Spans.NoSpan))

  def unapply(tree: AnnotatedType)(using Context): Option[(Type, ClassSymbol)] = tree match
    case AnnotatedType(parent: Type, ann) if ann.hasSymbol(defn.OnlyCapabilityAnnot) =>
      ann.tree.tpe.argTypes.head.classSymbol match
        case cls: ClassSymbol => Some((parent, cls))
        case _ => None
    case _ => None
end OnlyCapability

/** An extractor for all kinds of function types as well as method and poly types.
 *  It includes aliases of function types such as `=>`. TODO: Can we do without?
 *  @return  1st half: The argument types or empty if this is a type function
 *           2nd half: The result type
 */
object FunctionOrMethod:
  def unapply(tp: Type)(using Context): Option[(List[Type], Type)] = tp match
    case defn.FunctionOf(args, res, isContextual) => Some((args, res))
    case mt: MethodType => Some((mt.paramInfos, mt.resType))
    case mt: PolyType => Some((Nil, mt.resType))
    case defn.RefinedFunctionOf(rinfo) => unapply(rinfo)
    case _ => None

/** An extractor for a contains argument */
object ContainsImpl:
  def unapply(tree: TypeApply)(using Context): Option[(Tree, Tree)] =
    tree.fun.tpe.widen match
      case fntpe: PolyType if tree.fun.symbol == defn.Caps_containsImpl =>
        tree.args match
          case csArg :: refArg :: Nil => Some((csArg, refArg))
          case _ => None
      case _ => None

/** An extractor for a contains parameter */
object ContainsParam:
  def unapply(sym: Symbol)(using Context): Option[(TypeRef, Capability)] =
    sym.info.dealias match
      case AppliedType(tycon, (cs: TypeRef) :: arg2 :: Nil)
      if tycon.typeSymbol == defn.Caps_ContainsTrait
          && cs.typeSymbol.isAbstractOrParamType =>
        arg2.stripCapturing match // ref.type was converted to box ref.type^{ref} by boxing
          case ref: Capability => Some((cs, ref))
          case _ => None
      case _ => None

/** A class encapsulating the assumulator logic needed for `CaptureSet.ofTypeDeeply`
 *  and `derivesFromCapTraitDeeply`.
 *  NOTE: The traversal logic needs to be in sync with narrowCaps in CaptureOps, which
 *  replaces caps with reach capabilties. There are two exceptions, however.
 *   - First, invariant arguments. These have to be included to be conservative
 *     in dcs but must be excluded in narrowCaps.
 *   - Second, unconstrained type variables are handled specially in `ofTypeDeeply`.
 */
abstract class DeepTypeAccumulator[T](using Context) extends TypeAccumulator[T]:
  val seen = util.HashSet[Symbol]()

  protected def capturingCase(acc: T, parent: Type, refs: CaptureSet, boxed: Boolean): T

  protected def abstractTypeCase(acc: T, t: TypeRef, upperBound: Type): T

  def apply(acc: T, t: Type) =
    if variance < 0 then acc
    else t.dealias match
      case t @ CapturingType(parent, cs) =>
        capturingCase(acc, parent, cs, t.isBoxed)
      case t: TypeRef if t.symbol.isAbstractOrParamType && !seen.contains(t.symbol) =>
        seen += t.symbol
        abstractTypeCase(acc, t, t.info.bounds.hi)
      case AnnotatedType(parent, _) =>
        this(acc, parent)
      case t @ FunctionOrMethod(args, res) =>
        this(acc, res)
      case _ =>
        foldOver(acc, t)
end DeepTypeAccumulator

