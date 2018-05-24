import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Universe

case class Location(owners: List[String])

object Location {

  implicit inline def location: Location =
    ~impl(Universe.compilationUniverse) // FIXME infer Universe.compilationUniverse within top level ~

  def impl(implicit u: Universe): Expr[Location] = {
    import u._
    import u.tasty._

    def listOwnerNames(definition: Definition, acc: List[String]): List[String] = definition match {
      case ValDef(name, _, _) => listOwnerNames(definition.owner, name :: acc)
      case DefDef(name, _, _, _, _) => listOwnerNames(definition.owner, name :: acc)
      case ClassDef(name, _, _, _, _) => listOwnerNames(definition.owner, name :: acc)
      case _ => acc
    }

    val list = listOwnerNames(u.context.owner, Nil)
    '(new Location(~list.toExpr))
  }

  private implicit def ListIsLiftable[T : Liftable : Type]: Liftable[List[T]] = {
    case x :: xs  => '{ ~x.toExpr :: ~xs.toExpr }
    case Nil => '{ List.empty[T] }
  }
}
