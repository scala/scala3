import scala.quoted._

inline def old(inline x: Int): Int =
  ${ oldImpl(x) } // error

private def oldImpl(using s: Scope)(x: Int): s.Expr[Int] = ???

inline def `new`(inline x: Int): Int =
    ${ newImpl('x) }

private def newImpl(using s: Scope)(x: s.Expr[Int]): s.Expr[Int] = ???
