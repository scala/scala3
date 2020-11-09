import scala.quoted._

class A
class B extends A
class C extends B

inline def test(): Unit = ${ testExpr }

def testExpr(using QuoteContext): Expr[Unit] = {
  import qctx.reflect._

  '{
    println(${Expr(TypeRepr.of[Object].baseClasses.toString)})
    println(${Expr(TypeRepr.of[A].baseClasses.toString)})
    println(${Expr(TypeRepr.of[B].baseClasses.toString)})
    println(${Expr(TypeRepr.of[C].baseClasses.toString)})
  }
}
