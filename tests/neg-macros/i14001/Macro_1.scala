package test
import scala.quoted.*
object Y {
  inline def testStuff[T]: T =
    ${testStuffImpl} // problem: T inferred to Nothing
                     // solution add T explicitly

  def testStuffImpl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect.*

    Apply(
      Ref(Symbol.requiredMethod("test.A.C.apply")),
      List(Literal(IntConstant(1)))
    ).asExprOf[T]
  }
}
