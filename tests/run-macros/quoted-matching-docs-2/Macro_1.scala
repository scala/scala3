import scala.quoted.*


def sum(args: Int*): Int = args.sum

inline def showOptimize(inline arg: Int): String = ${ showOptimizeExpr('arg) }
inline def optimize(inline arg: Int): Int = ${ optimizeExpr('arg) }

private def showOptimizeExpr(body: Expr[Int])(using Quotes): Expr[String] =
  Expr(optimizeExpr(body).show)

private def optimizeExpr(body: Expr[Int])(using Quotes): Expr[Int] = body match {
  // Match a call to sum without any arguments
  case '{ sum() } => Expr(0)
  // Match a call to sum with an argument $n of type Int. n will be the Expr[Int] representing the argument.
  case '{ sum($n) } => n
  // Match a call to sum and extracts all its args in an `Expr[Seq[Int]]`
  case '{ sum(${Varargs(args)}*) } => sumExpr(args)
  case body => body
}

private def sumExpr(args1: Seq[Expr[Int]])(using Quotes): Expr[Int] = {
    def flatSumArgs(arg: Expr[Int]): Seq[Expr[Int]] = arg match {
      case '{ sum(${Varargs(subArgs)}*) } => subArgs.flatMap(flatSumArgs)
      case arg => Seq(arg)
    }
    val args2 = args1.flatMap(flatSumArgs)
    val staticSum: Int = args2.map(_.value.getOrElse(0)).sum
    val dynamicSum: Seq[Expr[Int]] = args2.filter(_.value.isEmpty)
    dynamicSum.foldLeft(Expr(staticSum))((acc, arg) => '{ $acc + $arg })
}
