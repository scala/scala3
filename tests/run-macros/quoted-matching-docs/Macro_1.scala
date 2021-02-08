import scala.quoted.*

inline def sum(args: Int*): Int = ${ sumExpr('args) }

inline def sumShow(args: Int*): String = ${ sumExprShow('args) }

private def sumExprShow(argsExpr: Expr[Seq[Int]]) (using Quotes): Expr[String] =
  Expr(sumExpr(argsExpr).show)

private def sumExpr(argsExpr: Expr[Seq[Int]])(using Quotes) : Expr[Int] = {
  UnsafeExpr.underlyingArgument(argsExpr) match {
    case Varargs(Exprs(args)) => // args is of type Seq[Int]
      Expr(args.sum) // precompute result of sum
    case Varargs(argExprs) => // argExprs is of type Seq[Expr[Int]]
      val staticSum: Int = argExprs.map(_.value.getOrElse(0)).sum
      val dynamicSum: Seq[Expr[Int]] = argExprs.filter(_.value.isEmpty)
      dynamicSum.foldLeft(Expr(staticSum))((acc, arg) => '{ $acc + $arg })
    case _ =>
      '{ $argsExpr.sum }
  }
}

object UnsafeExpr {
  def underlyingArgument[T](expr: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect.*
    expr.asTerm.underlyingArgument.asExpr.asInstanceOf[Expr[T]]
}