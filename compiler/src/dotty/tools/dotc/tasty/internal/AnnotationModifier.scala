package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Annotations.Annotation

import scala.tasty.modifiers

object AnnotationModifier {

  def apply(tree: Annotation): modifiers.Annotation = new Impl(tree)

  def unapplyAnnotation(arg: Impl)(implicit ctx: Context): Option[modifiers.Annotation.Data] = {
    Some(Term(arg.annot.tree))
  }

  private[tasty] class Impl(val annot: Annotation) extends modifiers.Annotation {
    override def toString: String = "Annotation"
  }
}
