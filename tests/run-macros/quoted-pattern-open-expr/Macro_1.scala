import scala.quoted._

inline def test(e: Int): String = ${testExpr('e)}

private def testExpr(e: Expr[Int])(using QuoteContext): Expr[String] = {
  e match {
    case '{ val y: Int = 4; $body } => Lifted("Matched closed\n" + body.show)
    case '{ val y: Int = 4; ($body: Int => Int)(y) } => Lifted("Matched open\n" + body.show)
    case '{ val y: Int => Int = x => x + 1; ($body: (Int => Int) => Int)(y) } => Lifted("Matched open\n" + body.show)
    case '{ def g(x: Int): Int = ($body: (Int => Int, Int) => Int)(g, x); g(5) } => Lifted("Matched open\n" + body.show)
  }
}
