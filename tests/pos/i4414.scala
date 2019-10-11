import scala.quoted._

object Test {
  given QuoteContext = ???

  def a[A: TypeTag](): Unit = {
    b[Expr[A]]()
    a[A]()
  }

  def b[A: TypeTag](): Unit = ???
}
