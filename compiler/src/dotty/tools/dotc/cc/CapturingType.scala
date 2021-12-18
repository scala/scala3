package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*

object CapturingType:

  def apply(parent: Type, refs: CaptureSet, boxed: Boolean)(using Context): Type =
    if refs.isAlwaysEmpty then parent
    else AnnotatedType(parent, CaptureAnnotation(refs, boxed))

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet, Boolean)] =
    if ctx.phase == Phases.checkCapturesPhase then EventuallyCapturingType.unapply(tp)
    else None

end CapturingType

object EventuallyCapturingType:

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet, Boolean)] =
    if tp.annot.symbol == defn.RetainsAnnot then
      tp.annot match
        case ann: CaptureAnnotation => Some((tp.parent, ann.refs, ann.boxed))
        case ann => Some((tp.parent, ann.tree.toCaptureSet, ann.tree.isBoxedCapturing))
    else None

end EventuallyCapturingType


