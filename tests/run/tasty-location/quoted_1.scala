import scala.quoted._

import scala.tasty._

case class Location(owners: List[String])

object Location {

  implicit rewrite def location: Location =
    ~impl(TopLevelSplice.tastyContext) // FIXME infer TopLevelSplice.tastyContext within top level ~

  def impl(implicit tasty: Tasty): Expr[Location] = {
    import tasty._

    def listOwnerNames(definition: Definition, acc: List[String]): List[String] = definition match {
      case ValDef(name, _, _) => listOwnerNames(definition.owner, name :: acc)
      case DefDef(name, _, _, _, _) => listOwnerNames(definition.owner, name :: acc)
      case ClassDef(name, _, _, _, _) => listOwnerNames(definition.owner, name :: acc)
      case _ => acc
    }

    val list = listOwnerNames(rootContext.owner, Nil)
    '(new Location(~list.toExpr))
  }

  private implicit def ListIsLiftable[T : Liftable : Type]: Liftable[List[T]] = {
    case x :: xs  => '{ ~x.toExpr :: ~xs.toExpr }
    case Nil => '{ List.empty[T] }
  }
}
