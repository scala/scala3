package scala.tasty
package trees

trait ImportSelector

object SimpleSelector {
  type Data = Id
  def unapply(arg: ImportSelector)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplySimpleSelector(arg)
}

object RenameSelector {
  type Data = (Id, Id)
  def unapply(arg: ImportSelector)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyRenameSelector(arg)
}

object OmitSelector {
  type Data = Id
  def unapply(arg: ImportSelector)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyOmitSelector(arg)
}
