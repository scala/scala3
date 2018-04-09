package scala.tasty
package names

trait SignedName extends PossiblySignedName

object SignedName {
  type Data = (TermName, TypeName, List[TypeName])
  def unapply(arg: SignedName)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplySignedName(arg)
}
