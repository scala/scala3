import scala.quoted._
object Test {

  type Contextual[T] = (s: Scope) ?=> T

  inline def i5_1[T](using s: Scope)(n: T): T = ${ foo('n) } // OK

  inline def i5_2[T](n: T): Contextual[T] = ${ foo('n) } // error: Macros using `X ?=> Y` return types are not yet supported

  def foo[T](using s: Scope)(x: s.Expr[T]) = x
}
