import scala.quoted._

class Var[T]

object Var {
  def apply[T, U](using s: Scope)(init: s.Expr[T])(body: Var[T] => s.Expr[U])(using s.Type[T], s.Type[U]): s.Expr[U] = '{
    var x = $init
    ${
      body(
        new Var[T] {
          def get(using s1: s.Nested): s1.Expr[T] = 'x
          def update(using s1: s.Nested)(e: s1.Expr[T]): s1.Expr[Unit] = '{ x = $e }
        }
      )
    }
  }
}
