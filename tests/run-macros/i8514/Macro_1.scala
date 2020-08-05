import scala.quoted._

class A
class B extends A
class C extends B

inline def test(): Unit = ${ testExpr }

def testExpr(using QuoteContext): Expr[Unit] = {
  import qctx.tasty._

  '{
    println(${Expr('[Object].asTypeTree.tpe.baseClasses.toString)})
    println(${Expr('[A].asTypeTree.tpe.baseClasses.toString)})
    println(${Expr('[B].asTypeTree.tpe.baseClasses.toString)})
    println(${Expr('[C].asTypeTree.tpe.baseClasses.toString)})
  }
}
