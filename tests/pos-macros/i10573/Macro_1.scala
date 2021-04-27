import scala.quoted.*


trait A { def foo: Int }
class B extends A { def foo: Int = 1 }

inline def test(): Unit = ${ testExpr() }

def testExpr()(using Quotes): Expr[Unit] = {
  val e0: Expr[A] = '{ new B }
  val e1: Expr[Int] = '{ $e0.foo }
  e1 match
    case '{ ($x: B).foo } => '{ val b: B = $x; () }
    case _ => quotes.reflect.report.throwError("did not match")
}

