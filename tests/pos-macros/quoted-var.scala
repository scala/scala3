import scala.quoted.*

class Var[T]

object Var {
  def apply[T: Type, U: Type](init: Expr[T])(body: Var[T] => Expr[U])(using Quotes): Expr[U] = '{
    var x = $init
    ${
      body(
        new Var[T] {
          def get(using Quotes): Expr[T] = 'x
          def update(e: Expr[T])(using Quotes): Expr[Unit] = '{ x = $e }
        }
      )
    }
  }
}
