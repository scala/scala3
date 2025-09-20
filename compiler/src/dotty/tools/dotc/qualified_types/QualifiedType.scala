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
  def unapply(tp: Type)(using Context): Option[(Type, ENode.Lambda)] =
    tp match
      case AnnotatedType(parent, QualifiedAnnotation(qualifier)) =>
        Some((parent, qualifier))
      case _ =>
        None

  def apply(parent: Type, qualifier: ENode.Lambda)(using Context): Type =
    AnnotatedType(parent, QualifiedAnnotation(qualifier))
