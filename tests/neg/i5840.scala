import scala.quoted._
object Test {

  type Contextual[T] = given QuoteContext => T

  inline def i5_1[T](n: T)(implicit thisCtx: QuoteContext): T = ${ foo('n) } // OK

  inline def i5_2[T](n: T): Contextual[T] = ${ foo('n) } // error: Macros using `given X => Y` return types are not yet supported

  def foo[T](x: Expr[T]) = x
}
