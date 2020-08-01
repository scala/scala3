import scala.quoted._

class A[+X[_], -Y]
class P[T]
class B extends A[P, String]

inline def test(): Unit = ${ testExpr }

def testExpr(using QuoteContext): Expr[Unit] = {
  import qctx.tasty._

  val t = '[B].unseal.tpe
  '{
    println(${Expr(t.baseClasses.map(b => t.baseType(b).toString))})
  }
}
