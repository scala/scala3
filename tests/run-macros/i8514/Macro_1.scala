import scala.quoted._

class A
class B extends A
class C extends B

inline def test(): Unit = ${ testExpr }

def testExpr(using QuoteContext): Expr[Unit] = {
  import qctx.tasty._

  '{
    println(${Expr(quoted.Type[Object].unseal.tpe.baseClasses.toString)})
    println(${Expr(quoted.Type[A].unseal.tpe.baseClasses.toString)})
    println(${Expr(quoted.Type[B].unseal.tpe.baseClasses.toString)})
    println(${Expr(quoted.Type[C].unseal.tpe.baseClasses.toString)})
  }
}
