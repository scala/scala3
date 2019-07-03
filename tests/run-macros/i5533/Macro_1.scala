import scala.quoted._

object scalatest {

  def f(x: Int): Boolean = false
  def f(x: String): Boolean = true

  inline def assert(condition: => Boolean): Unit = ${assertImpl('condition)}

  def assertImpl(condition: Expr[Boolean]) given (qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._

    val tree = condition.unseal

    val expr = tree.seal.cast[Boolean]

    '{println($expr)}
  }
}
