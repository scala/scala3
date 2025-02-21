package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import StdNames.nme
import ast.tpd.*
import Decorators.*
import typer.ErrorReporting.errorType
import Names.TermName
import NameKinds.ExistentialBinderName
import NameOps.isImpureFunction
import reporting.Message
import util.SimpleIdentitySet.empty
import CaptureSet.{Refs, emptyRefs, NarrowingCapabilityMap}
import dotty.tools.dotc.util.SimpleIdentitySet

/** A module for handling Fresh types. Fresh.Cap instances are top type that keep
 *  track of what they hide when capabilities get widened by subsumption to fresh.
 *  The module implements operations to convert between regular caps.cap and
 *  Fresh.Cap instances. Fresh.Cap is encoded as `caps.cap @freshCapability(...)` where
 *  `freshCapability(...)` is a special kind of annotation of type `Fresh.Annot`
 *  that contains a hidden set.
 */
object Fresh:

  /** The annotation of a Fresh.Cap instance */
  case class Annot(hidden: CaptureSet.HiddenSet) extends Annotation:
    override def symbol(using Context) = defn.FreshCapabilityAnnot
    override def tree(using Context) = New(symbol.typeRef, Nil)
    override def derivedAnnotation(tree: Tree)(using Context): Annotation = this

    override def hash: Int = hidden.hashCode
    override def eql(that: Annotation) = that match
      case Annot(hidden) => this.hidden eq hidden
      case _ => false
  end Annot

  /** The initial elements (either 0 or 1) of a hidden set created for given `owner`.
   *  If owner `x` is a trackable this is `x*` if reach` is true, or `x` otherwise.
   */
  private def ownerToHidden(owner: Symbol, reach: Boolean)(using Context): Refs =
    val ref = owner.termRef
    if reach then
      if ref.isTrackableRef then SimpleIdentitySet(ref.reach) else emptyRefs
    else
      if ref.isTracked then SimpleIdentitySet(ref) else emptyRefs

  /** An extractor for "fresh" capabilities */
  object Cap:

    def apply(owner: Symbol, initialHidden: Refs = emptyRefs)(using Context): CaptureRef =
      if ccConfig.useSepChecks then
        val hiddenSet = CaptureSet.HiddenSet(owner, initialHidden)
        val res = AnnotatedType(defn.captureRoot.termRef, Annot(hiddenSet))
        hiddenSet.owningCap = res
        //assert(hiddenSet.id != 3)
        res
      else
        defn.captureRoot.termRef

    def apply(owner: Symbol, reach: Boolean)(using Context): CaptureRef =
      apply(owner, ownerToHidden(owner, reach))

    def apply(owner: Symbol)(using Context): CaptureRef =
      apply(owner, ownerToHidden(owner, reach = false))

    def unapply(tp: AnnotatedType): Option[CaptureSet.HiddenSet] = tp.annot match
      case Annot(hidden) => Some(hidden)
      case _ => None
  end Cap

  /** Map each occurrence of cap to a different Sep.Cap instance */
  class FromCap(owner: Symbol)(using Context) extends BiTypeMap, FollowAliasesMap:
    thisMap =>

    private var reach = false

    override def apply(t: Type) =
      if variance <= 0 then t
      else t match
        case t: CaptureRef if t.isCap =>
          Cap(owner, ownerToHidden(owner, reach))
        case t @ CapturingType(_, refs) =>
          val savedReach = reach
          if t.isBoxed then reach = true
          try mapOver(t) finally reach = savedReach
        case t @ AnnotatedType(parent, ann) =>
          val parent1 = this(parent)
          if ann.symbol.isRetains && ann.tree.toCaptureSet.containsCap then
            this(CapturingType(parent1, ann.tree.toCaptureSet))
          else
            t.derivedAnnotatedType(parent1, ann)
        case _ =>
          mapFollowingAliases(t)

    override def toString = "CapToFresh"

    lazy val inverse: BiTypeMap & FollowAliasesMap = new BiTypeMap with FollowAliasesMap:
      def apply(t: Type): Type = t match
        case t @ Cap(_) => defn.captureRoot.termRef
        case t @ CapturingType(_, refs) => mapOver(t)
        case _ => mapFollowingAliases(t)

      def inverse = thisMap
      override def toString = thisMap.toString + ".inverse"

  end FromCap

  /** Maps cap to fresh */
  def fromCap(tp: Type, owner: Symbol = NoSymbol)(using Context): Type =
    if ccConfig.useSepChecks then FromCap(owner)(tp) else tp

  /** Maps fresh to cap */
  def toCap(tp: Type)(using Context): Type =
    if ccConfig.useSepChecks then FromCap(NoSymbol).inverse(tp) else tp

  /** If `refs` contains an occurrence of `cap` or `cap.rd`, the current context
   *  with an added property PrintFresh. This addition causes all occurrences of
   *  `Fresh.Cap` to be printed as `fresh` instead of `cap`, so that one avoids
   *  confusion in error messages.
   */
  def printContext(refs: (Type | CaptureSet)*)(using Context): Context =
    def hasCap = new TypeAccumulator[Boolean]:
      def apply(x: Boolean, t: Type) =
        x || t.dealiasKeepAnnots.match
          case Fresh.Cap(_) => false
          case t: TermRef => t.isCap || this(x, t.widen)
          case x: ThisType => false
          case _ => foldOver(x, t)
    def containsFresh(x: Type | CaptureSet): Boolean = x match
      case tp: Type =>
        hasCap(false, tp)
      case refs: CaptureSet =>
        refs.elems.exists(_.stripReadOnly.isCap)

    if refs.exists(containsFresh) then ctx.withProperty(PrintFresh, Some(()))
    else ctx
  end printContext
end Fresh





