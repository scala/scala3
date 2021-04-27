import scala.quoted.*

inline def eval(inline e: Int): Int = ${ evalExpr('e) }

private def evalExpr(using Quotes)(e: Expr[Int]): Expr[Int] = {
  e match {
    case '{ val y: Int = $x; $body(y): Int } =>
      evalExpr(Expr.betaReduce('{$body(${evalExpr(x)})}))
    case '{ ($x: Int) * ($y: Int) } =>
      (x.value, y.value) match
        case (Some(a), Some(b)) => Expr(a * b)
        case _ => e
    case _ => e
  }
}
