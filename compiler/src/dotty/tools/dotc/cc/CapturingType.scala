package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*

/** A capturing type. This is internally represented as an annotated type with a `retains`
 *  annotation, but the extractor will succeed only at phase CheckCaptures.
 *  Annotated types with `@retainsByName` annotation can also be created that way, by
 *  giving a `CapturingKind.ByName` as `kind` argument, but they are never extracted,
 *  since they have already been converted to regular capturing types before CheckCaptures.
 */
object CapturingType:

  def apply(parent: Type, refs: CaptureSet, kind: CapturingKind)(using Context): Type =
    if refs.isAlwaysEmpty then parent
    else AnnotatedType(parent, CaptureAnnotation(refs, kind))

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet, CapturingKind)] =
    if ctx.phase == Phases.checkCapturesPhase then
      val r = EventuallyCapturingType.unapply(tp)
      r match
        case Some((_, _, CapturingKind.ByName)) => None
        case _ => r
    else None

end CapturingType

/** An extractor for types that will be capturing types at phase CheckCaptures. Also
 *  included are types that indicate captures on enclosing call-by-name parameters
 *  before phase ElimByName
 */
object EventuallyCapturingType:

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet, CapturingKind)] =
    val sym = tp.annot.symbol
    if sym == defn.RetainsAnnot || sym == defn.RetainsByNameAnnot then
      tp.annot match
        case ann: CaptureAnnotation =>
          Some((tp.parent, ann.refs, ann.kind))
        case ann =>
          val kind =
            if ann.tree.isBoxedCapturing then CapturingKind.Boxed
            else if sym == defn.RetainsByNameAnnot then CapturingKind.ByName
            else CapturingKind.Regular
          try Some((tp.parent, ann.tree.toCaptureSet, kind))
          catch case ex: IllegalCaptureRef => None
    else None

end EventuallyCapturingType


