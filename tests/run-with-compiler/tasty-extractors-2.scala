
import scala.quoted._

import scala.tasty.statements._
import scala.tasty.terms._
import scala.tasty.constants._

import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.tasty.Toolbox._

object Test {
  def main(args: Array[String]): Unit = {
    val q = '{ var x = 1; x = 2 }
    println(show(toTasty(q)))

  }

  def show(tree: TopLevelStatement): String = tree match {
    case Block(stats, expr) => (stats ::: expr :: Nil).map(show).mkString("{ ", "; ", " }")
    case ValDef(name, tpt, rhs, mods) => s"val $name: $tpt${rhs.fold("")(rhs => " = " + show(rhs))}"
    case Assign(lhs, rhs) => s"${show(lhs)} = ${show(rhs)}"
    case Ident(name) => name.toString
    case Literal(const) => show(const)
//    case _ => "_"
  }

  def show(tree: Constant): String = tree.toString // TODO

}
