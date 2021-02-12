import scala.quoted.*
object Test {

  type Contextual[T] = Quotes ?=> T

  inline def i5_1[T](n: T)(implicit thisCtx: Quotes): T = ${ foo('n) } // OK

  inline def i5_2[T](n: T): Contextual[T] = ${ foo('n) } // error: Macros using `X ?=> Y` return types are not yet supported

  def foo[T](x: Expr[T]) = x
}
