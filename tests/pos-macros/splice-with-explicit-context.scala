import scala.quoted.*

def f(a: Expr[Int])(using q: Quotes): Unit =

  '{ val x: Int = ${ (q2) ?=> a } }

  '{ val x: Int = ${ (q2: q.Nested) ?=> a } }
