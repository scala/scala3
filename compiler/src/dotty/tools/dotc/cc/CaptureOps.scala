package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import ast.{tpd, untpd}
import Decorators.*, NameOps.*
import config.SourceVersion
import config.Printers.capt
import util.Property.Key
import tpd.*
import StdNames.nme
import config.Feature
import collection.mutable

private val Captures: Key[CaptureSet] = Key()
private val BoxedType: Key[BoxedTypeCache] = Key()

private val enableRootMapping = true

/** Switch whether unpickled function types and byname types should be mapped to
 *  impure types. With the new gradual typing using Fluid capture sets, this should
 *  be no longer needed. Also, it has bad interactions with pickling tests.
 */
private val adaptUnpickledFunctionTypes = false

/** The arguments of a @retains or @retainsByName annotation */
private[cc] def retainedElems(tree: Tree)(using Context): List[Tree] = tree match
  case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) => elems
  case _ => Nil

def allowUniversalInBoxed(using Context) =
  Feature.sourceVersion.isAtLeast(SourceVersion.`3.3`)

/** An exception thrown if a @retains argument is not syntactically a CaptureRef */
class IllegalCaptureRef(tpe: Type) extends Exception

/** Capture checking state, which is stored in a context property */
class CCState:

  /** Associates certain symbols (the nesting level owners) with their ccNestingLevel */
  val nestingLevels: mutable.HashMap[Symbol, Int] = new mutable.HashMap

  /** Associates nesting level owners with the local roots valid in their scopes. */
  val localRoots: mutable.HashMap[Symbol, Symbol] = new mutable.HashMap

  /** The last pair of capture reference and capture set where
   *  the reference could not be added to the set due to a level conflict.
   */
  var levelError: Option[(CaptureRef, CaptureSet)] = None
end CCState

/** Property key for capture checking state */
val ccStateKey: Key[CCState] = Key()

/** The currently valid CCState */
def ccState(using Context) = ctx.property(ccStateKey).get

trait FollowAliases extends TypeMap:
  def mapOverFollowingAliases(t: Type): Type = t match
    case t: LazyRef =>
      val t1 = this(t.ref)
      if t1 ne t.ref then t1 else t
    case _ =>
      val t1 = t.dealiasKeepAnnots
      if t1 ne t then
        val t2 = this(t1)
        if t2 ne t1 then return t2
      mapOver(t)

class mapRoots(from: CaptureRoot, to: CaptureRoot)(using Context) extends BiTypeMap, FollowAliases:
  thisMap =>

  def apply(t: Type): Type = t match
    case t: TermRef if (t eq from) && enableRootMapping =>
      to
    case t: CaptureRoot.Var =>
      val ta = t.followAlias
      if ta ne t then apply(ta)
      else from match
        case from: TermRef
        if t.upperLevel >= from.symbol.ccNestingLevel
          && CaptureRoot.isEnclosingRoot(from, t)
          && CaptureRoot.isEnclosingRoot(t, from) => to
        case from: CaptureRoot.Var if from.followAlias eq t => to
        case _ => from
    case _ =>
      mapOverFollowingAliases(t)

  def inverse = mapRoots(to, from)
end mapRoots

extension (tree: Tree)

  /** Map tree with CaptureRef type to its type, throw IllegalCaptureRef otherwise */
  def toCaptureRef(using Context): CaptureRef = tree.tpe match
    case ref: CaptureRef => ref
    case tpe => throw IllegalCaptureRef(tpe)

  /** Convert a @retains or @retainsByName annotation tree to the capture set it represents.
   *  For efficience, the result is cached as an Attachment on the tree.
   */
  def toCaptureSet(using Context): CaptureSet =
    tree.getAttachment(Captures) match
      case Some(refs) => refs
      case None =>
        val refs = CaptureSet(retainedElems(tree).map(_.toCaptureRef)*)
          .showing(i"toCaptureSet $tree --> $result", capt)
        tree.putAttachment(Captures, refs)
        refs

  /** Under pureFunctions, add a @retainsByName(*)` annotation to the argument of
   *  a by name parameter type, turning the latter into an impure by name parameter type.
   */
  def adaptByNameArgUnderPureFuns(using Context): Tree =
    if adaptUnpickledFunctionTypes && Feature.pureFunsEnabledSomewhere then
      val rbn = defn.RetainsByNameAnnot
      Annotated(tree,
        New(rbn.typeRef).select(rbn.primaryConstructor).appliedTo(
          Typed(
            SeqLiteral(ref(defn.captureRoot) :: Nil, TypeTree(defn.AnyType)),
            TypeTree(defn.RepeatedParamType.appliedTo(defn.AnyType))
          )
        )
      )
    else tree

extension (tp: Type)

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
        case ann: CaptureAnnotation =>
          ann.boxedType(tp)
        case ann =>
          ann.tree.getAttachment(BoxedType) match
            case None => ann.tree.putAttachment(BoxedType, BoxedTypeCache())
            case _ =>
          ann.tree.attachment(BoxedType)(tp)
    case tp: RealTypeBounds =>
      tp.derivedTypeBounds(tp.lo.boxed, tp.hi.boxed)
    case _ =>
      tp

  /** If `sym` is a type parameter, the boxed version of `tp`, otherwise `tp` */
  def boxedIfTypeParam(sym: Symbol)(using Context) =
    if sym.is(TypeParam) then tp.boxed else tp

  /** The boxed version of `tp`, unless `tycon` is a function symbol */
  def boxedUnlessFun(tycon: Type)(using Context) =
    if ctx.phase != Phases.checkCapturesPhase || defn.isFunctionSymbol(tycon.typeSymbol)
    then tp
    else tp.boxed

  /** The capture set consisting of all top-level captures of `tp` that appear under a box.
   *  Unlike for `boxed` this also considers parents of capture types, unions and
   *  intersections, and type proxies other than abstract types.
   */
  def boxedCaptureSet(using Context): CaptureSet =
    def getBoxed(tp: Type): CaptureSet = tp match
      case tp @ CapturingType(parent, refs) =>
        val pcs = getBoxed(parent)
        if tp.isBoxed then refs ++ pcs else pcs
      case tp: TypeRef if tp.symbol.isAbstractType => CaptureSet.empty
      case tp: TypeProxy => getBoxed(tp.superType)
      case tp: AndType => getBoxed(tp.tp1) ** getBoxed(tp.tp2)
      case tp: OrType => getBoxed(tp.tp1) ++ getBoxed(tp.tp2)
      case _ => CaptureSet.empty
    getBoxed(tp)

  /** Is the boxedCaptureSet of this type nonempty? */
  def isBoxedCapturing(using Context) = !tp.boxedCaptureSet.isAlwaysEmpty

  /** If this type is a capturing type, the version with boxed statues as given by `boxed`.
   *  If it is a TermRef of a capturing type, and the box status flips, widen to a capturing
   *  type that captures the TermRef.
   */
  def forceBoxStatus(boxed: Boolean)(using Context): Type = tp.widenDealias match
    case tp @ CapturingType(parent, refs) if tp.isBoxed != boxed =>
      val refs1 = tp match
        case ref: CaptureRef if ref.isTracked => ref.singletonCaptureSet
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

  /** Under pureFunctions, map regular function type to impure function type
   */
  def adaptFunctionTypeUnderPureFuns(using Context): Type = tp match
    case AppliedType(fn, args)
    if adaptUnpickledFunctionTypes && Feature.pureFunsEnabledSomewhere && defn.isFunctionClass(fn.typeSymbol) =>
      val fname = fn.typeSymbol.name
      defn.FunctionType(
        fname.functionArity,
        isContextual = fname.isContextFunction,
        isImpure = true).appliedTo(args)
    case _ =>
      tp

  /** Under pureFunctions, add a @retainsByName(*)` annotation to the argument of
   *  a by name parameter type, turning the latter into an impure by name parameter type.
   */
  def adaptByNameArgUnderPureFuns(using Context): Type =
    if adaptUnpickledFunctionTypes && Feature.pureFunsEnabledSomewhere then
      AnnotatedType(tp,
        CaptureAnnotation(CaptureSet.universal, boxed = false)(defn.RetainsByNameAnnot))
    else
      tp

  def isCapturingType(using Context): Boolean =
    tp match
      case CapturingType(_, _) => true
      case _ => false

  def isEventuallyCapturingType(using Context): Boolean =
    tp match
      case EventuallyCapturingType(_, _) => true
      case _ => false

  /** Is type known to be always pure by its class structure,
   *  so that adding a capture set to it would not make sense?
   */
  def isAlwaysPure(using Context): Boolean = tp.dealias match
    case tp: (TypeRef | AppliedType) =>
      val sym = tp.typeSymbol
      if sym.isClass then sym.isPureClass
      else tp.superType.isAlwaysPure
    case CapturingType(parent, refs) =>
      parent.isAlwaysPure || refs.isAlwaysEmpty
    case tp: TypeProxy =>
      tp.superType.isAlwaysPure
    case tp: AndType =>
      tp.tp1.isAlwaysPure || tp.tp2.isAlwaysPure
    case tp: OrType =>
      tp.tp1.isAlwaysPure && tp.tp2.isAlwaysPure
    case _ =>
      false
/*!!!
  def capturedLocalRoot(using Context): Symbol =
    tp.captureSet.elems.toList
      .filter(_.isLocalRootCapability)
      .map(_.termSymbol)
      .maxByOption(_.ccNestingLevel)
      .getOrElse(NoSymbol)

  /** Remap roots defined in `cls` to the ... */
  def remapRoots(pre: Type, cls: Symbol)(using Context): Type =
    if cls.isStaticOwner then tp
    else
      val from =
        if cls.source == ctx.compilationUnit.source then cls.localRoot
        else defn.captureRoot
      mapRoots(from, capturedLocalRoot)(tp)


  def containsRoot(root: Symbol)(using Context): Boolean =
    val search = new TypeAccumulator[Boolean]:
      def apply(x: Boolean, t: Type): Boolean =
        if x then true
        else t.dealias match
          case t1: TermRef if t1.symbol == root => true
          case t1: TypeRef if t1.classSymbol.hasAnnotation(defn.CapabilityAnnot) => true
          case t1: MethodType =>
            !foldOver(x, t1.paramInfos) && this(x, t1.resType)
          case t1 @ AppliedType(tycon, args) if defn.isFunctionSymbol(tycon.typeSymbol) =>
            val (inits, last :: Nil) = args.splitAt(args.length - 1): @unchecked
            !foldOver(x, inits) && this(x, last)
          case t1 => foldOver(x, t1)
    search(false, tp)
*/

extension (cls: ClassSymbol)

  def pureBaseClass(using Context): Option[Symbol] =
    cls.baseClasses.find(bc =>
      defn.pureBaseClasses.contains(bc)
      || {
        val selfType = bc.givenSelfType
        selfType.exists && selfType.captureSet.isAlwaysEmpty
      })

extension (sym: Symbol)

  /** A class is pure if:
   *   - one its base types has an explicitly declared self type with an empty capture set
   *   - or it is a value class
   *   - or it is an exception
   *   - or it is one of Nothing, Null, or String
   */
  def isPureClass(using Context): Boolean = sym match
    case cls: ClassSymbol =>
      cls.pureBaseClass.isDefined || defn.pureSimpleClasses.contains(cls)
    case _ =>
      false

  /** Does this symbol allow results carrying the universal capability?
   *  Currently this is true only for function type applies (since their
   *  results are unboxed) and `erasedValue` since this function is magic in
   *  that is allows to conjure global capabilies from nothing (aside: can we find a
   *  more controlled way to achieve this?).
   *  But it could be generalized to other functions that so that they can take capability
   *  classes as arguments.
   */
  def allowsRootCapture(using Context): Boolean =
    sym == defn.Compiletime_erasedValue
    || defn.isFunctionClass(sym.maybeOwner)

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
    && sym != defn.Caps_unsafeBox
    && sym != defn.Caps_unsafeUnbox

  def isLevelOwner(using Context): Boolean =
    def isCaseClassSynthetic =
      sym.owner.isClass && sym.owner.is(Case) && sym.is(Synthetic) && sym.info.firstParamNames.isEmpty
    if sym.isClass then true
    else if sym.is(Method) then
      if sym.isAnonymousFunction then
        // Setup added anonymous functions counting as level owners to nestingLevels
        ccState.nestingLevels.contains(sym)
      else
        !sym.isConstructor && !isCaseClassSynthetic
    else false

  /** The owner of the current level. Qualifying owners are
   *   - methods other than constructors and anonymous functions
   *   - anonymous functions, provided they either define a local
   *     root of type caps.Root, or they are the rhs of a val definition.
   *   - classes, if they are not staticOwners
   *   - _root_
   */
  def levelOwner(using Context): Symbol =
    if !sym.exists || sym.isRoot || sym.isStaticOwner then defn.RootClass
    else if sym.isLevelOwner then sym
    else sym.owner.levelOwner

  /** The nesting level of `sym` for the purposes of `cc`,
   *  -1 for NoSymbol
   */
  def ccNestingLevel(using Context): Int =
    if sym.exists then
      val lowner = sym.levelOwner
      ccState.nestingLevels.getOrElseUpdate(lowner,
        if lowner.isRoot then 0 else lowner.owner.ccNestingLevel + 1)
    else -1

  /** Optionally, the nesting level of `sym` for the purposes of `cc`, provided
   *  a capture checker is running.
   */
  def ccNestingLevelOpt(using Context): Option[Int] =
    if ctx.property(ccStateKey).isDefined then Some(ccNestingLevel) else None

  def setNestingLevel(level: Int)(using Context): Unit =
    ccState.nestingLevels(sym) = level

  /** The parameter with type caps.Root in the leading term parameter section,
   *  or NoSymbol, if none exists.
   */
  def definedLocalRoot(using Context): Symbol =
    sym.paramSymss.dropWhile(psyms => psyms.nonEmpty && psyms.head.isType) match
      case psyms :: _ => psyms.find(_.info.typeSymbol == defn.Caps_Root).getOrElse(NoSymbol)
      case _ => NoSymbol

  def localRoot(using Context): Symbol =
    val owner = sym.levelOwner
    assert(owner.exists)
    def newRoot = newSymbol(if owner.isClass then newLocalDummy(owner) else owner,
      nme.LOCAL_CAPTURE_ROOT, Synthetic, defn.Caps_Root.typeRef, nestingLevel = owner.ccNestingLevel)
    def lclRoot =
      if owner.isTerm then owner.definedLocalRoot.orElse(newRoot)
      else newRoot
    ccState.localRoots.getOrElseUpdate(owner, lclRoot)

  def maxNested(other: Symbol)(using Context): Symbol =
    if sym.ccNestingLevel < other.ccNestingLevel then other else sym
    /* does not work yet, we do mix sets with different levels, for instance in cc-this.scala.
    else if sym.ccNestingLevel > other.ccNestingLevel then sym
    else
      assert(sym == other, i"conflicting symbols at same nesting level: $sym, $other")
      sym
    */

  def minNested(other: Symbol)(using Context): Symbol =
    if sym.ccNestingLevel > other.ccNestingLevel then other else sym

extension (tp: TermRef | ThisType)
  /** The nesting level of this reference as defined by capture checking */
  def ccNestingLevel(using Context): Int = tp match
    case tp: TermRef => tp.symbol.ccNestingLevel
    case tp: ThisType => tp.cls.ccNestingLevel

extension (tp: AnnotatedType)
  /** Is this a boxed capturing type? */
  def isBoxed(using Context): Boolean = tp.annot match
    case ann: CaptureAnnotation => ann.boxed
    case _ => false

extension (ts: List[Type])
  /** Equivalent to ts.mapconserve(_.boxedUnlessFun(tycon)) but more efficient where
   *  it is the identity.
   */
  def boxedUnlessFun(tycon: Type)(using Context) =
    if ctx.phase != Phases.checkCapturesPhase || defn.isFunctionClass(tycon.typeSymbol)
    then ts
    else ts.mapconserve(_.boxed)

