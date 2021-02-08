import scala.quoted.*

inline def old(inline x: Int): Int =
  ${ oldImpl(x) } // error

private def oldImpl(x: Int): Expr[Int] = ???

inline def `new`(inline x: Int): Int =
    ${ newImpl('x) }

private def newImpl(x: Expr[Int]): Expr[Int] = ???
