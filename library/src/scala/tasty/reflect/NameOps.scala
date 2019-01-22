package scala.tasty.reflect

trait NameOps extends Core {

  /** Semantic name of a term */
  val TermName: TermNameModule
  abstract class TermNameModule {
    def unapply(name: TermName)(implicit ctx: Context): Option[String]
  }

  trait TermNameAPI {
  }
  implicit def TermNameDeco(name: TermName): TermNameAPI

  /** Semantic name of a type */
  val TypeName: TypeNameModule
  abstract class TypeNameModule {
    def unapply(name: TypeName)(implicit ctx: Context): Option[String]
  }

  trait TypeNameAPI {
  }
  implicit def TypeNameDeco(name: TypeName): TypeNameAPI
}
