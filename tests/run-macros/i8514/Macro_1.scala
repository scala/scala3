import scala.quoted._

class A
class B extends A
class C extends B

inline def test(): Unit = ${ testExpr }

def testExpr(using Quotes): Expr[Unit] = {
  import quotes.reflect._

  '{
    println(${Value(TypeRepr.of[Object].baseClasses.toString)})
    println(${Value(TypeRepr.of[A].baseClasses.toString)})
    println(${Value(TypeRepr.of[B].baseClasses.toString)})
    println(${Value(TypeRepr.of[C].baseClasses.toString)})
  }
}
