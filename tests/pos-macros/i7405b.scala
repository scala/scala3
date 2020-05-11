import scala.quoted._

class Foo {
  def f(using s: Scope): s.Expr[Any] = {
    '{
      trait X {
        type Y
        def y: Y = ???
      }
      val x: X = ???
      type Z = x.Y
      ${
        val t: scope.Type[Z] = '[Z]
        '{ val y: $t = x.y }
      }
    }
  }
}
