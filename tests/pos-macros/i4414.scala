import scala.quoted._

object Test {
  given QuoteContext = ???

  def a[A: Staged](): Unit = {
    b[Expr[A]]()
    a[A]()
  }

  def b[A: Staged](): Unit = ???
}
