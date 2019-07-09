import scala.quoted._

class C {
    type T = Int
    def fn(e : Expr[T]) given QuoteContext: Expr[T] = '{ println(); $e }
}
