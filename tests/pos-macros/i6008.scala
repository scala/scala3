import scala.quoted.*

class C {
    type T = Int
    def fn(e : Expr[T])(using Quotes): Expr[T] = '{ println(); $e }
}
