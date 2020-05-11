import scala.quoted._

def testImpl(using s: Scope)(f: s.Expr[(Int, Int) => Int]): s.Expr[Int] = Expr.betaReduce('{$f(1, 2)})

inline def test(f: (Int, Int) => Int) = ${
  testImpl('f)
}
