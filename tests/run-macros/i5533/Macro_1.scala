import scala.quoted.*

object scalatest {

  def f(x: Int): Boolean = false
  def f(x: String): Boolean = true

  inline def assert(condition: => Boolean): Unit = ${assertImpl('condition)}

  def assertImpl(condition: Expr[Boolean])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    val tree = condition.asTerm

    val expr = tree.asExprOf[Boolean]

    '{println($expr)}
  }
}
