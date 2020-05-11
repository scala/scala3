import scala.quoted._

class A
class B extends A
class C extends B

inline def test(): Unit = ${ testExpr }

def testExpr(using Scope): scope.Expr[Unit] = {
  import scope.tasty._

  '{
    println(${Expr('[Object].tpe.baseClasses.toString)})
    println(${Expr('[A].tpe.baseClasses.toString)})
    println(${Expr('[B].tpe.baseClasses.toString)})
    println(${Expr('[C].tpe.baseClasses.toString)})
  }
}
