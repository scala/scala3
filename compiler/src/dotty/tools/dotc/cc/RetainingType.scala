package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*
import ast.tpd.*
import Annotations.Annotation
import Decorators.i

/** A builder and extractor for annotated types with @retains or @retainsByName annotations.
 */
object RetainingType:

  def apply(tp: Type, refs: List[Tree], byName: Boolean = false)(using Context): Type =
    val annotCls = if byName then defn.RetainsByNameAnnot else defn.RetainsAnnot
    val annotTree =
      New(annotCls.typeRef,
        Typed(
          SeqLiteral(refs, TypeTree(defn.AnyType)),
          TypeTree(defn.RepeatedParamClass.typeRef.appliedTo(defn.AnyType))) :: Nil)
    AnnotatedType(tp, Annotation(annotTree))

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, List[Tree])] =
    val sym = tp.annot.symbol
    if sym == defn.RetainsAnnot || sym == defn.RetainsByNameAnnot then
      tp.annot match
        case _: CaptureAnnotation =>
          assert(ctx.mode.is(Mode.IgnoreCaptures), s"bad retains $tp at ${ctx.phase}")
          None
        case ann =>
          Some((tp.parent, ann.tree.retainedElems))
    else
      None
end RetainingType
