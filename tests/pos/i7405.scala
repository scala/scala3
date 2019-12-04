import scala.quoted._
class Foo {
  def f(given QuoteContext): Expr[Any] = {
    '{
      type X = Int // Level 1
      val x: X = ???
      ${
        val t: Type[X] = '[X] // Level 0
        '{ val y: $t = x }
      }
    }
  }
}
