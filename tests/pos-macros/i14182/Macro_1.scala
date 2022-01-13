import scala.quoted._
def fooImpl(xs: Expr[(Int, Int)])(using Quotes): Expr[Unit] =
  '{ val a: Int = $xs._1; }
