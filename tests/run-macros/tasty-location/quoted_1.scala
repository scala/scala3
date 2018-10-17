import scala.quoted._
import scala.quoted.autolift._

import scala.tasty._

case class Location(owners: List[String])

object Location {

  implicit inline def location: Location = ${impl}

  def impl(implicit staging: StagingContext): Expr[Location] = {
    import staging.reflection._

    def listOwnerNames(sym: Symbol, acc: List[String]): List[String] =
      if (sym == definitions.RootClass || sym == definitions.EmptyPackageClass) acc
      else listOwnerNames(sym.owner, sym.name :: acc)

    val list = listOwnerNames(rootContext.owner, Nil)
    '{new Location(${list})}
  }

  private implicit def ListIsLiftable[T : Liftable : Type]: Liftable[List[T]] = new Liftable[List[T]] {
    override def toExpr(x: List[T])(implicit st: StagingContext): Expr[List[T]] = x match {
      case x :: xs  => '{ ${x} :: ${xs} }
      case Nil => '{ List.empty[T] }
    }
  }
}
