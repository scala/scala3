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

/** A trait for references in CaptureSets. These can be NamedTypes, ThisTypes or ParamRefs,
 *  as well as two kinds of AnnotatedTypes representing reach and maybe capabilities.
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

  /** Is this a reach reference of the form `x*`? */
  final def isReach(using Context): Boolean = this match
    case AnnotatedType(_, annot) => annot.symbol == defn.ReachCapabilityAnnot
    case _ => false

  /** Is this a maybe reference of the form `x?`? */
  final def isMaybe(using Context): Boolean = this match
    case AnnotatedType(_, annot) => annot.symbol == defn.MaybeCapabilityAnnot
    case _ => false

  final def stripReach(using Context): CaptureRef =
    if isReach then
      val AnnotatedType(parent: CaptureRef, _) = this: @unchecked
      parent
    else this

  final def stripMaybe(using Context): CaptureRef =
    if isMaybe then
      val AnnotatedType(parent: CaptureRef, _) = this: @unchecked
      parent
    else this

  /** Is this reference the generic root capability `cap` ? */
  final def isRootCapability(using Context): Boolean = this match
    case tp: TermRef => tp.name == nme.CAPTURE_ROOT && tp.symbol == defn.captureRoot
    case _ => false

  /** Is this reference capability that does not derive from another capability ? */
  final def isMaxCapability(using Context): Boolean = this match
    case tp: TermRef => tp.isRootCapability || tp.info.derivesFrom(defn.Caps_Exists)
    case tp: TermParamRef => tp.underlying.derivesFrom(defn.Caps_Exists)
    case _ => false

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
      if !isCaptureChecking || underlying.isProvisional then
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
   *   TODO: Document cases with more comments.
   */
  final def subsumes(y: CaptureRef)(using Context): Boolean =

    def subsumingRefs(x: Type, y: Type): Boolean = x match
      case x: CaptureRef => y match
        case y: CaptureRef => x.subsumes(y)
        case _ => false
      case _ => false

    def viaInfo(info: Type)(test: Type => Boolean): Boolean = info.match
      case info: SingletonCaptureRef => test(info)
      case info: AndType => viaInfo(info.tp1)(test) || viaInfo(info.tp2)(test)
      case info: OrType => viaInfo(info.tp1)(test) && viaInfo(info.tp2)(test)
      case _ => false

    (this eq y)
    || this.isRootCapability
    || y.match
        case y: TermRef =>
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
        case y: TypeRef if y.derivesFrom(defn.Caps_CapSet) =>
          y.info match
            case TypeBounds(_, hi: CaptureRef) => this.subsumes(hi)
            case _ => y.captureSetOfInfo.elems.forall(this.subsumes)
        case AnnotatedType(parent, ann)
        if ann.symbol.isRetains && parent.derivesFrom(defn.Caps_CapSet) =>
          ann.tree.toCaptureSet.elems.forall(this.subsumes)
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
        case AnnotatedType(parent, ann)
        if ann.symbol.isRetains && parent.derivesFrom(defn.Caps_CapSet) =>
          ann.tree.toCaptureSet.elems.exists(_.subsumes(y))
        case _ => false
  end subsumes

  def assumedContainsOf(x: TypeRef)(using Context): SimpleIdentitySet[CaptureRef] =
    CaptureSet.assumedContains.getOrElse(x, SimpleIdentitySet.empty)

end CaptureRef

trait SingletonCaptureRef extends SingletonType, CaptureRef

