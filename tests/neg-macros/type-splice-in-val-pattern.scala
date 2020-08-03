import scala.quoted._
object Foo {
  def f(using q: QuoteContext) = {
    val t: Staged[Int] = ???
    val '[ *:[$t] ] = ??? // error
  }
}