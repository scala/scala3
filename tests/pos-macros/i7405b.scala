import scala.quoted.*

class Foo {
  def f(using Quotes): Expr[Any] = {
    '{
      trait X extends A {
        type Y
        def y: Y = ???
      }
      val x: X = ???
      type Z = x.Y
      ${
        val t: Type[Z] = Type.of[Z]
        '{ val y: Z = x.y }
        '{ val y: t.Underlying = x.y }
      }
    }
  }
}

trait A:
  type Y
  def y: Y = ???
