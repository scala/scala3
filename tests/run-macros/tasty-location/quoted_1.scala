import scala.quoted._
import scala.quoted.autolift._

import scala.tasty._

case class Location(owners: List[String])

object Location {

  implicit inline def location: Location = ${impl}

  def impl(implicit reflect: Reflection): Expr[Location] = {
    import reflect._

    def listOwnerNames(sym: Symbol, acc: List[String]): List[String] =
      if (sym == definitions.RootClass || sym == definitions.EmptyPackageClass) acc
      else listOwnerNames(sym.owner, sym.name :: acc)

    val list = listOwnerNames(rootContext.owner, Nil)
    '{new Location(${list})}
  }

  private implicit def ListIsLiftable[T : Liftable : Type]: Liftable[List[T]] = {
    case x :: xs  => '{ ${x} :: ${xs} }
    case Nil => '{ List.empty[T] }
  }
}
