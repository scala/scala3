import scala.quoted._

object Macros {

  inline def foreach1(start: Int, end: Int, f: Int => Unit): String = ${impl('start, 'end, 'f)}
  inline def foreach2(start: Int, end: Int, f: => Int => Unit): String = ${impl('start, 'end, 'f)}
  inline def foreach3(start: Int, end: Int, inline f: Int => Unit): String = ${impl('start, 'end, 'f)}

  def impl(using s: Scope)(start: s.Expr[Int], end: s.Expr[Int], f: s.Expr[Int => Unit]): s.Expr[String] = {
    import s.tasty._
    val res = '{
      var i = $start
      val j = $end
      while (i < j) {
        ${Expr.betaReduce('{$f(i)})}
        i += 1
      }
      while {
        ${Expr.betaReduce('{$f(i)})}
        i += 1
        i < j
      } do ()
    }
    Expr(res.show)
  }
}
