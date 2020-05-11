import scala.quoted._

inline def test(f: (Int, Int) => Int) = ${
  testImpl(
    (a: scope.Expr[Int], b: scope.Expr[Int]) =>  '{ f(${a}, ${b}) } // error: Malformed macro
  )
}

def testImpl(using s: Scope)(f: (s.Expr[Int], s.Expr[Int]) => s.Expr[Int]): s.Expr[Int] = ???
