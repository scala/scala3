import scala.quoted._

object scalatest {

  def f(x: Int): Boolean = false
  def f(x: String): Boolean = true

  inline def assert(condition: => Boolean): Unit = ${assertImpl('condition)}

  def assertImpl(condition: Expr[Boolean])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._

    val tree = Term.of(condition)

    val expr = tree.asExprOf[Boolean]

    '{println($expr)}
  }
}
