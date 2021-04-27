import scala.quoted.*

object Test {
  given Quotes = ???

  def a[A: Type](): Unit = {
    b[Expr[A]]()
    a[A]()
  }

  def b[A: Type](): Unit = ???
}
