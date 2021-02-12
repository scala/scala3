import scala.quoted.*

inline def test(f: (Int, Int) => Int) = ${
  testImpl(
    (a: Expr[Int], b: Expr[Int]) =>  '{ f(${a}, ${b}) } // error: Malformed macro
  )
}

def testImpl(f: (Expr[Int], Expr[Int]) => Expr[Int]): Expr[Int] = ???
