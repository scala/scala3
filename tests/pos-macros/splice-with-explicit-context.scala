import scala.quoted._

def f(a: Expr[Int])(using q: Quotes): Unit =

  '{ val x: Int = ${ (using q2) => a } }

  '{ val x: Int = ${ (using q2: q.Nested) => a } }
