import scala.quoted.*
object Test {
  def res(x: quoted.Expr[Int])(using Quotes): quoted.Expr[Int] = x match {
    case '{ val a: Int = $y; 1} => y                     // owner of `y` is `res`
    case _ => '{ val b: Int = ${val c = 2; Expr(c)}; 1} // owner of `c` is `b`, but that seems to be OK
  }
}