import scala.quoted._

class A
class B extends A
class C extends B

inline def test(): Unit = ${ testExpr }

def testExpr(using QuoteContext): Expr[Unit] = {
  import qctx.reflect._

  '{
    println(${Expr('[Object].unseal.tpe.baseClasses.toString)})
    println(${Expr('[A].unseal.tpe.baseClasses.toString)})
    println(${Expr('[B].unseal.tpe.baseClasses.toString)})
    println(${Expr('[C].unseal.tpe.baseClasses.toString)})
  }
}
