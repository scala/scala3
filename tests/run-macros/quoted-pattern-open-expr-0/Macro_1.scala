import scala.quoted._

inline def test(inline e: Int): String = ${testExpr('e)}

private def testExpr(using s: Scope)(e: s.Expr[Int]): s.Expr[String] = {
  e match {
    case '{ val y: Int = 4; $body(y): Int } => Expr("Matched open\n" + body.show)
  }
}
