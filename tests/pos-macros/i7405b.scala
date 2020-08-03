import scala.quoted._

class Foo {
  def f(using QuoteContext): Expr[Any] = {
    '{
      trait X {
        type Y
        def y: Y = ???
      }
      val x: X = ???
      type Z = x.Y
      ${
        val t: Staged[Z] = '[Z]
        '{ val y: $t = x.y }
      }
    }
  }
}
