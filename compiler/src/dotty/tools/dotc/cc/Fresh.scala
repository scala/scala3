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

  object Cap:
    def apply(initialHidden: Refs = emptySet)(using Context): AnnotatedType =
      AnnotatedType(defn.captureRoot.termRef, Annot(CaptureSet.HiddenSet(initialHidden)))

    def unapply(tp: AnnotatedType)(using Context): Option[CaptureSet.HiddenSet] = tp.annot match
      case Annot(hidden) => Some(hidden)
      case _ => None
  end Cap

  class FromCap(initialHidden: Refs = emptySet)(using Context) extends BiTypeMap:
    thisMap =>

    var change = false

    override def apply(t: Type) =
      if variance <= 0 then t
      else t.dealiasKeepAnnots match
        case t: CaptureRef if t.isCap =>
          change = true
          Cap(initialHidden)
        case CapturingType(_, v: CaptureSet.Var) =>
          change = true
          mapOver(t)
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
  def fromCap(tp: Type, initialHidden: Refs = emptySet)(using Context): Type =
    val mapper = FromCap(initialHidden)
    val mapped = mapper(tp)
    if mapper.change then mapped else tp

  def fromCap(tp: Type, initialHidden: CaptureRef)(using Context): Type =
    fromCap(tp, SimpleIdentitySet(initialHidden))

  def fromCap(info: Type, sym: Symbol)(using Context): Type =
    val initHidden =
      if sym.exists && sym.termRef.isTracked then SimpleIdentitySet(sym.termRef)
      else emptySet
    fromCap(info, initHidden)
end Fresh





