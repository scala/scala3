import scala.quoted._

class Var[T]

object Var {
  def apply[T: Staged, U: Staged](init: Expr[T])(body: Var[T] => Expr[U])(using qctx: QuoteContext): Expr[U] = '{
    var x = $init
    ${
      body(
        new Var[T] {
          def get(using qctx: QuoteContext): Expr[T] = 'x
          def update(e: Expr[T])(using qctx: QuoteContext): Expr[Unit] = '{ x = $e }
        }
      )
    }
  }
}
