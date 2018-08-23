import scala.quoted._

import scala.tasty._

case class Location(owners: List[String])

object Location {

  implicit transparent def location: Location =
    ~impl(TopLevelSplice.tastyContext) // FIXME infer TopLevelSplice.tastyContext within top level ~

  def impl(implicit tasty: Tasty): Expr[Location] = {
    import tasty._

    def listOwnerNames(sym: Symbol, acc: List[String]): List[String] =
      if (sym == definitions.RootClass || sym == definitions.EmptyPackageClass) acc
      else listOwnerNames(sym.owner, sym.name :: acc)

    val list = listOwnerNames(rootContext.owner, Nil)
    '(new Location(~list.toExpr))
  }

  private implicit def ListIsLiftable[T : Liftable : Type]: Liftable[List[T]] = {
    case x :: xs  => '{ ~x.toExpr :: ~xs.toExpr }
    case Nil => '{ List.empty[T] }
  }
}
