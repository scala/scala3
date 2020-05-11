import scala.quoted._

object scalatest {

  def f(x: Int): Boolean = false
  def f(x: String): Boolean = true

  inline def assert(condition: => Boolean): Unit = ${assertImpl('condition)}

  def assertImpl(using s: Scope)(condition: s.Expr[Boolean]): s.Expr[Unit] = {
    import s.tasty._

    val tree = condition

    val expr = tree.seal.cast[Boolean]

    '{println($expr)}
  }
}
