import scala.quoted._
import scala.tasty._

object scalatest {

  inline def assert(x: => Any): Unit = ${ assertImpl('x) }

  def assertImpl(x: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._
    x.unseal.underlyingArgument
    '{ () }
  }
}
