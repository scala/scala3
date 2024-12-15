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
import CaptureSet.{Refs, emptySet, NarrowingCapabilityMap}
import dotty.tools.dotc.util.SimpleIdentitySet

/** Handling fresh in CC:

*/
object Fresh:

  case class Annot(hidden: CaptureSet.HiddenSet) extends Annotation:
    override def symbol(using Context) = defn.FreshCapabilityAnnot
    override def tree(using Context) = New(symbol.typeRef, Nil)
    override def derivedAnnotation(tree: Tree)(using Context): Annotation = this

    override def hash: Int = hidden.hashCode
    override def eql(that: Annotation) = that match
      case Annot(hidden) => this.hidden eq hidden
      case _ => false
  end Annot

  private def ownerToHidden(owner: Symbol, reach: Boolean)(using Context): Refs =
    val ref = owner.termRef
    if reach then
      if ref.isTrackableRef then SimpleIdentitySet(ref.reach) else emptySet
    else
      if ref.isTracked then SimpleIdentitySet(ref) else emptySet

  object Cap:

    def apply(initialHidden: Refs = emptySet)(using Context): CaptureRef =
      if ccConfig.useFresh then
        AnnotatedType(defn.captureRoot.termRef, Annot(CaptureSet.HiddenSet(initialHidden)))
      else
        defn.captureRoot.termRef

    def apply(owner: Symbol, reach: Boolean)(using Context): CaptureRef =
      apply(ownerToHidden(owner, reach))

    def apply(owner: Symbol)(using Context): CaptureRef =
      apply(ownerToHidden(owner, reach = false))

    def unapply(tp: AnnotatedType)(using Context): Option[CaptureSet.HiddenSet] = tp.annot match
      case Annot(hidden) => Some(hidden)
      case _ => None
  end Cap

  class FromCap(owner: Symbol)(using Context) extends BiTypeMap:
    thisMap =>

    var change = false
    var reach = false

    private def initHidden =
      val ref = owner.termRef
      if reach then
        if ref.isTrackableRef then SimpleIdentitySet(ref.reach) else emptySet
      else
        if ref.isTracked then SimpleIdentitySet(ref) else emptySet

    override def apply(t: Type) =
      if variance <= 0 then t
      else t.dealiasKeepAnnots match
        case t: CaptureRef if t.isCap =>
          change = true
          Cap(initHidden)
        case t @ CapturingType(_, refs) =>
          change = refs.isInstanceOf[CaptureSet.Var]
          val savedReach = reach
          if t.isBoxed then reach = true
          try mapOver(t) finally reach = savedReach
        case _ =>
          mapOver(t)

    override def toString = "CapToFresh"

    lazy val inverse = new BiTypeMap:
      def apply(t: Type): Type = t match
        case t @ Cap(_) => defn.captureRoot.termRef
        case _ => mapOver(t)
      def inverse = thisMap
      override def toString = thisMap.toString + ".inverse"

  end FromCap

  /** Maps cap to fresh */
  def fromCap(tp: Type, owner: Symbol)(using Context): Type =
    if ccConfig.useFresh then
      val mapper = FromCap(owner)
      val mapped = mapper(tp)
      if mapper.change then mapped else tp
    else
      tp
/*
  def fromCap(tp: CaptureRef, initialHidden: Refs)(using Context): CaptureRef =
    fromCap(tp: Type, initialHidden).asInstanceOf[CaptureRef]

  def fromCap(tp: Type, initialHidden: CaptureRef)(using Context): Type =
    fromCap(tp, SimpleIdentitySet(initialHidden))

  def fromCap(info: Type, owner: Symbol)(using Context): CaptureRef =
    val ref = sym.termRef
    val initHidden =
      if reach then
        if ref.isTrackableRef then SimpleIdentitySet(ref.reach) else emptySet
      else
        if ref.isTracked then SimpleIdentitySet(ref) else emptySet
    fromCap(info, initHidden)*/
end Fresh





