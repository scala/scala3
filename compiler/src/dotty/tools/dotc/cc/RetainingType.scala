package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*

/** A builder and extractor for annotated types with @retains or @retainsByName annotations
 *  excluding CapturingTypes.
 */
object RetainingType:

  def apply(tp: Type, typeElems: Type, byName: Boolean = false)(using Context): Type =
    val annotCls = if byName then defn.RetainsByNameAnnot else defn.RetainsAnnot
    AnnotatedType(tp, RetainingAnnotation(annotCls.typeRef.appliedTo(typeElems)))

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, Type)] = tp.annot match
    case ann: RetainingAnnotation => Some((tp.parent, ann.retainedType))
    case _ => None
end RetainingType
