package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Decorators.*
import util.{SimpleIdentitySet, Property}
import typer.ErrorReporting.Addenda
import TypeComparer.subsumesExistentially
import util.common.alwaysTrue
import scala.collection.mutable
import CCState.*
import Periods.NoRunId
import compiletime.uninitialized
import StdNames.nme
import CaptureSet.VarState

/** A trait for references in CaptureSets. These can be NamedTypes, ThisTypes or ParamRefs,
 *  as well as three kinds of AnnotatedTypes representing readOnly, reach, and maybe capabilities.
 *  If there are several annotations they come with an orderL
 *  `*` first, `.rd` next, `?` last.
 */
trait CaptureRef extends TypeProxy, ValueType:
  private var myCaptureSet: CaptureSet | Null = uninitialized
  private var myCaptureSetRunId: Int = NoRunId
  private var mySingletonCaptureSet: CaptureSet.Const | Null = null

  /** Is the reference tracked? This is true if it can be tracked and the capture
   *  set of the underlying type is not always empty.
   */
  final def isTracked(using Context): Boolean =
    this.isTrackableRef && (isMaxCapability || !captureSetOfInfo.isAlwaysEmpty)

  /** Is this a maybe reference of the form `x?`? */
  final def isMaybe(using Context): Boolean = this ne stripMaybe

  /** Is this a read-only reference of the form `x.rd` or a capture set variable
   *  with only read-ony references in its upper bound?
   */
  final def isReadOnly(using Context): Boolean = this match
    case tp: TypeRef => tp.captureSetOfInfo.isReadOnly
    case _ => this ne stripReadOnly

  /** Is this a reach reference of the form `x*`? */
  final def isReach(using Context): Boolean = this ne stripReach

  final def stripMaybe(using Context): CaptureRef = this match
    case AnnotatedType(tp1: CaptureRef, annot) if annot.symbol == defn.MaybeCapabilityAnnot =>
      tp1
    case _ =>
      this

  final def stripReadOnly(using Context): CaptureRef = this match
    case tp @ AnnotatedType(tp1: CaptureRef, annot) =>
      val sym = annot.symbol
      if sym == defn.ReadOnlyCapabilityAnnot then
        tp1
      else if sym == defn.MaybeCapabilityAnnot then
        tp.derivedAnnotatedType(tp1.stripReadOnly, annot)
      else
        this
    case _ =>
      this

  final def stripReach(using Context): CaptureRef = this match
    case tp @ AnnotatedType(tp1: CaptureRef, annot) =>
      val sym = annot.symbol
      if sym == defn.ReachCapabilityAnnot then
        tp1
      else if sym == defn.ReadOnlyCapabilityAnnot || sym == defn.MaybeCapabilityAnnot then
        tp.derivedAnnotatedType(tp1.stripReach, annot)
      else
        this
    case _ =>
      this

  /** Is this reference the generic root capability `cap` ? */
  final def isCap(using Context): Boolean = this match
    case tp: TermRef => tp.name == nme.CAPTURE_ROOT && tp.symbol == defn.captureRoot
    case _ => false

  /** Is this reference a Fresh.Cap instance? */
  final def isFresh(using Context): Boolean = this match
    case Fresh.Cap(_) => true
    case _ => false

  /** Is this reference the generic root capability `cap` or a Fresh.Cap instance? */
  final def isCapOrFresh(using Context): Boolean = isCap || isFresh

  /** Is this reference one the generic root capabilities `cap` or `cap.rd` ? */
  final def isRootCapability(using Context): Boolean = this match
    case ReadOnlyCapability(tp1) => tp1.isCapOrFresh
    case _ => isCapOrFresh

  /** Is this reference capability that does not derive from another capability ? */
  final def isMaxCapability(using Context): Boolean = this match
    case tp: TermRef => tp.isCap || tp.info.derivesFrom(defn.Caps_Exists)
    case tp: TermParamRef => tp.underlying.derivesFrom(defn.Caps_Exists)
    case Fresh.Cap(_) => true
    case ReadOnlyCapability(tp1) => tp1.isMaxCapability
    case _ => false

  final def isExclusive(using Context): Boolean =
    !isReadOnly && (isMaxCapability || captureSetOfInfo.isExclusive)

  // With the support of pathes, we don't need to normalize the `TermRef`s anymore.
  // /** Normalize reference so that it can be compared with `eq` for equality */
  // final def normalizedRef(using Context): CaptureRef = this match
  //   case tp @ AnnotatedType(parent: CaptureRef, annot) if tp.isTrackableRef =>
  //     tp.derivedAnnotatedType(parent.normalizedRef, annot)
  //   case tp: TermRef if tp.isTrackableRef =>
  //     tp.symbol.termRef
  //   case _ => this

  /** The capture set consisting of exactly this reference */
  final def singletonCaptureSet(using Context): CaptureSet.Const =
    if mySingletonCaptureSet == null then
      mySingletonCaptureSet = CaptureSet(this)
    mySingletonCaptureSet.uncheckedNN

  /** The capture set of the type underlying this reference */
  final def captureSetOfInfo(using Context): CaptureSet =
    if ctx.runId == myCaptureSetRunId then myCaptureSet.nn
    else if myCaptureSet.asInstanceOf[AnyRef] eq CaptureSet.Pending then CaptureSet.empty
    else
      myCaptureSet = CaptureSet.Pending
      val computed = CaptureSet.ofInfo(this)
      if !isCaptureChecking || ctx.mode.is(Mode.IgnoreCaptures) || underlying.isProvisional then
        myCaptureSet = null
      else
        myCaptureSet = computed
        myCaptureSetRunId = ctx.runId
      computed

  final def invalidateCaches() =
    myCaptureSetRunId = NoRunId

  /**  x subsumes x
   *   x =:= y       ==>  x subsumes y
   *   x subsumes y  ==>  x subsumes y.f
   *   x subsumes y  ==>  x* subsumes y, x subsumes y?
   *   x subsumes y  ==>  x* subsumes y*, x? subsumes y?
   *   x: x1.type /\ x1 subsumes y  ==>  x subsumes y
   *   X = CapSet^cx, exists rx in cx, rx subsumes y     ==>  X subsumes y
   *   Y = CapSet^cy, forall ry in cy, x subsumes ry     ==>  x subsumes Y
   *   X: CapSet^c1...CapSet^c2, (CapSet^c1) subsumes y  ==>  X subsumes y
   *   Y: CapSet^c1...CapSet^c2, x subsumes (CapSet^c2)  ==>  x subsumes Y
   *   Contains[X, y]  ==>  X subsumes y
   *
   *   TODO: Move to CaptureSet
   */
  final def subsumes(y: CaptureRef)(using ctx: Context, vs: VarState = VarState.Separate): Boolean =

    def subsumingRefs(x: Type, y: Type): Boolean = x match
      case x: CaptureRef => y match
        case y: CaptureRef => x.subsumes(y)
        case _ => false
      case _ => false

    def viaInfo(info: Type)(test: Type => Boolean): Boolean = info.dealias match
      case info: SingletonCaptureRef => test(info)
      case CapturingType(parent, _) => viaInfo(parent)(test)
      case info: AndType => viaInfo(info.tp1)(test) || viaInfo(info.tp2)(test)
      case info: OrType => viaInfo(info.tp1)(test) && viaInfo(info.tp2)(test)
      case _ => false

    (this eq y)
    || maxSubsumes(y, canAddHidden = !vs.isOpen)
    || y.match
        case y: TermRef if !y.isCap =>
            y.prefix.match
              case ypre: CaptureRef =>
                this.subsumes(ypre)
                || this.match
                    case x @ TermRef(xpre: CaptureRef, _) if x.symbol == y.symbol =>
                      // To show `{x.f} <:< {y.f}`, it is important to prove `x` and `y`
                      // are equvalent, which means `x =:= y` in terms of subtyping,
                      // not just `{x} =:= {y}` in terms of subcapturing.
                      // It is possible to construct two singleton types `x` and `y`,
                      // which subsume each other, but are not equal references.
                      // See `tests/neg-custom-args/captures/path-prefix.scala` for example.
                      withMode(Mode.IgnoreCaptures) {TypeComparer.isSameRef(xpre, ypre)}
                    case _ =>
                      false
              case _ => false
          || viaInfo(y.info)(subsumingRefs(this, _))
        case MaybeCapability(y1) => this.stripMaybe.subsumes(y1)
        case ReadOnlyCapability(y1) => this.stripReadOnly.subsumes(y1)
        case y: TypeRef if y.derivesFrom(defn.Caps_CapSet) =>
          // The upper and lower bounds don't have to be in the form of `CapSet^{...}`.
          // They can be other capture set variables, which are bounded by `CapSet`,
          // like `def test[X^, Y^, Z >: X <: Y]`.
          y.info match
            case TypeBounds(_, hi: CaptureRef) => this.subsumes(hi)
            case _ => y.captureSetOfInfo.elems.forall(this.subsumes)
        case CapturingType(parent, refs) if parent.derivesFrom(defn.Caps_CapSet) =>
          refs.elems.forall(this.subsumes)
        case _ => false
    || this.match
        case ReachCapability(x1) => x1.subsumes(y.stripReach)
        case x: TermRef => viaInfo(x.info)(subsumingRefs(_, y))
        case x: TermParamRef => subsumesExistentially(x, y)
        case x: TypeRef if assumedContainsOf(x).contains(y) => true
        case x: TypeRef if x.derivesFrom(defn.Caps_CapSet) =>
          x.info match
            case TypeBounds(lo: CaptureRef, _) =>
              lo.subsumes(y)
            case _ =>
              x.captureSetOfInfo.elems.exists(_.subsumes(y))
        case CapturingType(parent, refs) if parent.derivesFrom(defn.Caps_CapSet) =>
          refs.elems.exists(_.subsumes(y))
        case _ => false
  end subsumes

  /** This is a maximal capabaility that subsumes `y` in given context and VarState.
   *  @param canAddHidden  If true we allow maximal capabilties to subsume all other capabilities.
   *                       We add those capabilities to the hidden set if this is Fresh.Cap
   *                       If false we only accept `y` elements that are already in the
   *                       hidden set of this Fresh.Cap. The idea is that in a VarState that
   *                       accepts additions we first run `maxSubsumes` with `canAddHidden = false`
   *                       so that new variables get added to the sets. If that fails, we run
   *                       the test again with canAddHidden = true as a last effort before we
   *                       fail a comparison.
   */
  def maxSubsumes(y: CaptureRef, canAddHidden: Boolean)(using ctx: Context, vs: VarState = VarState.Separate): Boolean =
    this.match
      case Fresh.Cap(hidden) =>
        vs.ifNotSeen(this)(hidden.elems.exists(_.subsumes(y)))
        || !y.stripReadOnly.isCap && canAddHidden && vs.addHidden(hidden, y)
      case _ =>
        this.isCap && canAddHidden
        || y.match
            case ReadOnlyCapability(y1) => this.stripReadOnly.maxSubsumes(y1, canAddHidden)
            case _ => false

  def assumedContainsOf(x: TypeRef)(using Context): SimpleIdentitySet[CaptureRef] =
    CaptureSet.assumedContains.getOrElse(x, SimpleIdentitySet.empty)

end CaptureRef

trait SingletonCaptureRef extends SingletonType, CaptureRef

