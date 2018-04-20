package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Decorators.sourcePos

import scala.tasty.modifiers

object AnnotationModifier {

  def apply(tree: Annotation)(implicit ctx: Context): modifiers.Modifier = new Impl(tree)

  def unapplyAnnotation(arg: Impl): Option[modifiers.Annotation.Data] = {
    implicit val ctx: Context = arg.ctx
    Some(Term(arg.annot.tree))
  }

  private[tasty] class Impl(val annot: Annotation)(implicit val ctx: Context) extends modifiers.Modifier {

    def pos: scala.tasty.Position = new Position(sourcePos(annot.tree(ctx).pos)(ctx))

    override def toString: String = {
      import Toolbox.extractor
      val modifiers.Annotation(annot) = this
      s"Annotation($annot)"
    }
  }
}
