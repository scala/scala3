import scala.quoted._
object Macros {
  def impl(self: Expr[StringContext])(using Quotes): Expr[String] = self match {
    case '{ StringContext() } => '{""}
    case '{ StringContext($part1) } => part1
    case '{ StringContext($part1, $part2) } => '{ $part1 + $part2 }
    case '{ StringContext($parts: _*) } => '{ $parts.mkString }
  }
}
