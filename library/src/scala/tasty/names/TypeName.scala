package scala.tasty
package names

trait TypeName extends Name

object TypeName {
  type Data = TermName
  def unapply(arg: TypeName)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTypeName(arg)
}
