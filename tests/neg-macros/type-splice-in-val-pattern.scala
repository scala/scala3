import scala.quoted._
object Foo {
  def f(using q: QuoteContext) = {
    val t: Type[Int] = ???
    val '[ *:[$t] ] = ??? // error
  }
}