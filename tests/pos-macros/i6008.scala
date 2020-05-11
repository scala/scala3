import scala.quoted._

class C {
    type T = Int
    def fn(using s: Scope)(e: s.Expr[T]): s.Expr[T] = '{ println(); $e }
}
