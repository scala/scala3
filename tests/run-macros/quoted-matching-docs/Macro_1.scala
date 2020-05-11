import scala.quoted._

inline def sum(args: Int*): Int = ${ sumExpr('args) }

inline def sumShow(args: Int*): String = ${ sumExprShow('args) }

private def sumExprShow(using s: Scope)(argsExpr: s.Expr[Seq[Int]]): s.Expr[String] =
  Expr(sumExpr(argsExpr).show)

private def sumExpr(using s: Scope)(argsExpr: s.Expr[Seq[Int]]): s.Expr[Int] = {
  import s.tasty._
  argsExpr.underlyingArgument.seal.cast[Seq[Int]] match {
    case Varargs(Consts(args)) => // args is of type Seq[Int]
      Expr(args.sum) // precompute result of sum
    case Varargs(argExprs) => // argExprs is of type Seq[Expr[Int]]
      val staticSum: Int = argExprs.map {
        case Const(arg) => arg
        case _ => 0
      }.sum
      val dynamicSum: Seq[s.Expr[Int]] = argExprs.filter {
        case Const(_) => false
        case arg => true
      }
      dynamicSum.foldLeft(Expr(staticSum))((acc, arg) => '{ $acc + $arg })
    case _ =>
      '{ $argsExpr.sum }
  }
}