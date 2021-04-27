import scala.quoted.*

class A
class B extends A
class C extends B

inline def test(): Unit = ${ testExpr }

def testExpr(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  '{
    println(${Expr(TypeRepr.of[Object].baseClasses.toString)})
    println(${Expr(TypeRepr.of[A].baseClasses.toString)})
    println(${Expr(TypeRepr.of[B].baseClasses.toString)})
    println(${Expr(TypeRepr.of[C].baseClasses.toString)})
  }
}
