import scala.quoted.*

inline def test1(inline x: Int): String = ${ test1Expr('x) }
inline def test2(inline x: Int): String = ${ test2Expr('x) }

private def test1Expr(x: Expr[Int])(using Quotes) : Expr[String] =
  x match
    case '{ val x: Int = 1; $z: Int } => Expr(z.show)
    case '{ val x: Int = 1; $z(x): Int } => Expr('{$z(1)}.show)
    case _ => '{"No match"}

private def test2Expr(x: Expr[Int])(using Quotes) : Expr[String] =
  x match
    case '{ val x: Int = 1; val y: Int = 2; $z: Int } => Expr(z.show)
    case '{ val x: Int = 1; val y: Int = 2; $z(x): Int } => Expr('{$z(1)}.show)
    case '{ val x: Int = 1; val y: Int = 2; $z(y): Int } => Expr('{$z(2)}.show)
    case '{ val x: Int = 1; val y: Int = 2; $z(x,y): Int } => Expr('{$z(1, 2)}.show)
    case _ => '{"No match"}
