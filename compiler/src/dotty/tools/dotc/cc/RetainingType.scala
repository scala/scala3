package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*
import ast.tpd.*
import Annotations.Annotation
import Decorators.i

/** A builder and extractor for annotated types with @retains or @retainsByName annotations
 *  excluding CapturingTypes.
 */
object RetainingType:

  def apply(tp: Type, typeElems: Type, byName: Boolean = false)(using Context): Type =
    val annotCls = if byName then defn.RetainsByNameAnnot else defn.RetainsAnnot
    val annotTree = New(AppliedType(annotCls.typeRef, typeElems :: Nil), Nil)
    AnnotatedType(tp, Annotation(annotTree))

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, Type)] =
    val sym = tp.annot.symbol
    if sym.isRetainsLike then
      tp.annot match
        case _: CaptureAnnotation => None
        case ann => Some((tp.parent, ann.tree.retainedSet))
    else
      None
end RetainingType
