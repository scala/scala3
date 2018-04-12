package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Decorators.sourcePos

import scala.tasty.modifiers

object AnnotationModifier {

  def apply(tree: Annotation)(implicit ctx: Context): modifiers.Modifier = Impl(tree, ctx)

  def unapplyAnnotation(mod: modifiers.Modifier): Option[modifiers.Annotation.Data] = mod match {
    case Impl(annot, ctx) => Some(Term(annot.tree(ctx))(ctx))
    case _ => None
  }

  private case class Impl(annot: Annotation, ctx: Context) extends modifiers.Modifier {

    def pos: scala.tasty.Position = new Position(sourcePos(annot.tree(ctx).pos)(ctx))

    override def toString: String = {
      import Toolbox.extractor
      val modifiers.Annotation(annot) = this
      s"Annotation($annot)"
    }
  }
}
