import scala.quoted._

object Test {
  given as QuoteContext = ???

  def a[A: Type](): Unit = {
    b[Expr[A]]()
    a[A]()
  }

  def b[A: Type](): Unit = ???
}
