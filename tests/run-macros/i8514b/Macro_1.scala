import scala.quoted.*

class A[+X[_], -Y]
class P[T]
class B extends A[P, String]

inline def test(): Unit = ${ testExpr }

def testExpr(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  val t = TypeRepr.of[B]
  val baseTypes = t.baseClasses.map(b => t.baseType(b))

  '{
    println(${Expr(baseTypes.map(_.show).mkString("\n"))})
  }
}
