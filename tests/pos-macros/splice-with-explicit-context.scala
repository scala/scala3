import scala.quoted._

def f(using s: Scope)(a: s.Expr[Int]): Unit =

  '{ val x: Int = ${ (using s2) => a } }

  '{ val x: Int = ${ (using s2: s.Nested) => a } }
