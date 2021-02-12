import scala.quoted.*

def testImpl(f: Expr[(Int, Int) => Int])(using Quotes): Expr[Int] = Expr.betaReduce('{$f(1, 2)})

inline def test(f: (Int, Int) => Int) = ${
  testImpl('f)
}
