import scala.quoted._

inline def test(f: (Int, Int) => Int) = ${ // error: Malformed macro
  testImpl((a: Expr[Int], b: Expr[Int]) =>  '{ f(${a}, ${b}) })
}

def testImpl(f: (Expr[Int], Expr[Int]) => Expr[Int]): Expr[Int] = ???
