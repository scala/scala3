import scala.quoted._
class Foo {
  def f(using s: Scope): s.Expr[Any] = {
    '{
      type X = Int // Level 1
      val x: X = ???
      ${
        val t: scope.Type[X] = '[X] // Level 0
        '{ val y: $t = x }
      }
    }
  }
}
