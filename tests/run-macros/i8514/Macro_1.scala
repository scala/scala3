import scala.quoted._

class A
class B extends A
class C extends B

inline def test(): Unit = ${ testExpr }

def testExpr(using QuoteContext): Expr[Unit] = {
  import qctx.reflect._

  '{
    println(${Expr(Type[Object].unseal.tpe.baseClasses.toString)})
    println(${Expr(Type[A].unseal.tpe.baseClasses.toString)})
    println(${Expr(Type[B].unseal.tpe.baseClasses.toString)})
    println(${Expr(Type[C].unseal.tpe.baseClasses.toString)})
  }
}
