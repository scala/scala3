import scala.quoted.{Expr, Quotes}

object Macro:
  inline def decorate(inline s: String): String = ${ decorateQuotes('s) }
  def decorateQuotes(s: Expr[String])(using Quotes): Expr[String] = '{ ">>> " + $s + " <<<" }

object Greeting:
  def greet() = "hello"
