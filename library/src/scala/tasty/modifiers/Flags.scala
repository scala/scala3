package scala.tasty
package modifiers

trait Flags extends Modifier

object Flags {
  type Data = FlagSet
  def unapply(arg: Flags)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyFlags(arg)
}
