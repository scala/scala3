import scala.quoted.*

inline def test(inline e: Int): String = ${testExpr('e)}

private def testExpr(e: Expr[Int])(using Quotes): Expr[String] = {
  e match {
    case '{ val y: Int = 4; $body(y): Int } => Expr("Matched open\n" + body.show)
  }
}
