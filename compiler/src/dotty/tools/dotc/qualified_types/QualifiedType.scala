package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Types.{AnnotatedType, Type}

/** A qualified type is internally represented as a type annotated with a
 *  `@qualified` annotation.
 */
object QualifiedType:
  /** Extractor for qualified types.
   *
   *  @param tp
   *    the type to deconstruct
   *  @return
   *    a pair containing the parent type and the qualifier tree (a lambda) on
   *    success, [[None]] otherwise
   */
  def unapply(tp: Type)(using Context): Option[(Type, tpd.Tree)] =
    tp match
      case AnnotatedType(parent, annot) if annot.symbol == ctx.definitions.QualifiedAnnot =>
        Some((parent, annot.argument(0).get))
      case _ =>
        None

  /** Factory method to create a qualified type.
   *
   *  @param parent
   *    the parent type
   *  @param qualifier
   *    the qualifier tree (a lambda)
   *  @return
   *    a qualified type
   */
  def apply(parent: Type, qualifier: tpd.Tree)(using Context): Type =
    val annotTp = ctx.definitions.QualifiedAnnot.typeRef.appliedTo(parent)
    AnnotatedType(parent, Annotation(tpd.New(annotTp, List(qualifier))))
