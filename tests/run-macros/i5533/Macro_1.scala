import scala.quoted._
import scala.tasty._

object scalatest {

  def f(x: Int): Boolean = false
  def f(x: String): Boolean = true

  inline def assert(condition: => Boolean): Unit = ${assertImpl('condition)}

  def assertImpl(condition: Expr[Boolean])(implicit refl: Reflection): Expr[Unit] = {
    import refl._

    val tree = condition.unseal

    val expr = tree.seal.cast[Boolean]

    '{println($expr)}
  }
}
