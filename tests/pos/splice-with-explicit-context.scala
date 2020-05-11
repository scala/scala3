import scala.quoted._

def f(a: Expr[Int])(using qctx: QuoteContext): Unit =

  '{ val x: Int = ${ (using qctx2) => a } }

  '{ val x: Int = ${ (using qctx2: qctx.Nested) => a } }
