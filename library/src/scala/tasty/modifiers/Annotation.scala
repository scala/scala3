package scala.tasty
package modifiers

trait Annotation extends Modifier

object Annotation {
  type Data = trees.Term
  def unapply(arg: Annotation)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyAnnotation(arg)
}
