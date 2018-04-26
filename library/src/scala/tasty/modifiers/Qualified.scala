package scala.tasty
package modifiers

trait Qualified extends Modifier

object QualifiedPrivate {
  type Data = types.Type
  def unapply(arg: Qualified)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyQualifiedPrivate(arg)
}

object QualifiedProtected {
  type Data = types.Type
  def unapply(arg: Qualified)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyQualifiedProtected(arg)
}
