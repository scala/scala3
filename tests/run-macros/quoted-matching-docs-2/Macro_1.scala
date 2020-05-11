import scala.quoted._


def sum(args: Int*): Int = args.sum

inline def showOptimize(inline arg: Int): String = ${ showOptimizeExpr('arg) }
inline def optimize(inline arg: Int): Int = ${ optimizeExpr('arg) }

private def showOptimizeExpr(using s: Scope)(body: s.Expr[Int]): s.Expr[String] =
  Expr(optimizeExpr(body).show)

private def optimizeExpr(using s: Scope)(body: s.Expr[Int]): s.Expr[Int] = body match {
  // Match a call to sum without any arguments
  case '{ sum() } => Expr(0)
  // Match a call to sum with an argument $n of type Int. n will be the Expr[Int] representing the argument.
  case '{ sum($n) } => n
  // Match a call to sum and extracts all its args in an `Expr[Seq[Int]]`
  case '{ sum(${Varargs(args)}: _*) } => sumExpr(args)
  case body => body
}

private def sumExpr(using s: Scope)(args1: Seq[s.Expr[Int]]): s.Expr[Int] = {
    def flatSumArgs(arg: s.Expr[Int]): Seq[s.Expr[Int]] = arg match {
      case '{ sum(${Varargs(subArgs)}: _*) } => subArgs.flatMap(flatSumArgs)
      case arg => Seq(arg)
    }
    val args2 = args1.flatMap(flatSumArgs)
    val staticSum: Int = args2.map {
      case Const(arg) => arg
      case _ => 0
    }.sum
    val dynamicSum: Seq[s.Expr[Int]] = args2.filter {
      case Const(_) => false
      case arg => true
    }
    dynamicSum.foldLeft(Expr(staticSum))((acc, arg) => '{ $acc + $arg })
}
