import scala.quoted._
import scala.tasty.Reflection
object Macros {
  def impl(self: Expr[StringContext]) given Reflection: Expr[String] = self match {
    case '{ StringContext() } => '{""}
    case '{ StringContext($part1) } => part1
    case '{ StringContext($part1, $part2) } => '{ $part1 + $part2 }
    case '{ StringContext($parts: _*) } => '{ $parts.mkString }
  }
}
