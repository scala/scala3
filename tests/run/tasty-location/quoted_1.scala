import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Context
import scala.tasty.names._
import scala.tasty.trees._

case class Location(owners: List[String])

object Location {

  implicit inline def location: Location =
    ~impl(Context.compilationContext) // FIXME infer Context.compilationContext within top level ~

  def impl(implicit ctx: Context): Expr[Location] = {
    val list = listOwnerNames(ctx.owner, Nil)(ctx)
    '(new Location(~list.toExpr))
  }

  private def listOwnerNames(definition: Definition, acc: List[String])(implicit ctx: Context): List[String] = definition match {
    case ValDef(name, _, _) => listOwnerNames(definition.owner, nameString(name) :: acc)
    case DefDef(name, _, _, _, _) => listOwnerNames(definition.owner, nameString(name) :: acc)
    case ClassDef(name, _, _, _, _) => listOwnerNames(definition.owner, nameString(name) :: acc)
    case _ => acc
  }

  private def nameString(name: Name)(implicit ctx: Context): String = name match {
    case Simple(nme) => nme
    case ObjectClass(underlying) => nameString(underlying)
    case TypeName(underlying) => nameString(underlying)
  }

  private implicit def ListIsLiftable[T : Liftable : Type]: Liftable[List[T]] = {
    case x :: xs  => '{ ~x.toExpr :: ~xs.toExpr }
    case Nil => '{ List.empty[T] }
  }
}
