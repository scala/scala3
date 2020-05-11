import scala.quoted._

object scalatest {

  inline def assert(x: => Any): Unit = ${ assertImpl('x) }

  def assertImpl(using s: Scope)(x: s.Expr[Any]): s.Expr[Unit] = {
    import s.tasty._
    x.underlyingArgument
    '{ () }
  }
}
