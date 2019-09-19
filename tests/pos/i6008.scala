import scala.quoted.{_, given}

class C {
    type T = Int
    def fn(e : Expr[T])(given QuoteContext): Expr[T] = '{ println(); $e }
}
