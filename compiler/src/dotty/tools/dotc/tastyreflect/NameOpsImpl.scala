package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Decorators._

trait NameOpsImpl extends scala.tasty.reflect.NameOps with CoreImpl {

  object TermName extends TermNameModule {
    def apply(x: String)(implicit ctx: Context): TermName = x.toTermName
    def unapply(x: TermName)(implicit ctx: Context): Option[String] = Some(x.toString)
  }

  def TermNameDeco(sig: TermName): TermNameAPI = new TermNameAPI {
  }

  object TypeName extends TypeNameModule {
    def apply(x: String)(implicit ctx: Context): TypeName = x.toTypeName
    def unapply(x: TypeName)(implicit ctx: Context): Option[String] = Some(x.toString)
  }

  def TypeNameDeco(sig: TypeName): TypeNameAPI = new TypeNameAPI {
  }
}
