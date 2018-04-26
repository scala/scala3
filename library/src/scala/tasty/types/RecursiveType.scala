package scala.tasty
package types

trait RecursiveType extends Type

object RecursiveType {
  type Data = Type
  def unapply(arg: RecursiveType)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyRecursiveType(arg)
}
