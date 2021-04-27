import scala.quoted.*
class Foo {
  def f(using Quotes): Expr[Any] = {
    '{
      type X = Int // Level 1
      val x: X = ???
      ${
        val t: Type[X] = Type.of[X] // Level 0
        '{ val y: t.Underlying = x }
      }
    }
  }
}
