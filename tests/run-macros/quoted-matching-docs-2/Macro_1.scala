import scala.quoted._
import scala.quoted.matching._

def sum(args: =>Int*): Int = args.sum

inline def showOptimize(arg: Int): String = ${ showOptimizeExpr('arg) }
inline def optimize(arg: Int): Int = ${ optimizeExpr('arg) }

private def showOptimizeExpr(body: Expr[Int])(given QuoteContext): Expr[String] =
  Expr(optimizeExpr(body).show)

private def optimizeExpr(body: Expr[Int])(given QuoteContext): Expr[Int] = body match {
  // Match a call to sum without any arguments
  case '{ sum() } => Expr(0)
  // Match a call to sum with an argument $n of type Int. n will be the Expr[Int] representing the argument.
  case '{ sum($n) } => n
  // Match a call to sum and extracts all its args in an `Expr[Seq[Int]]`
  case '{ sum(${ExprSeq(args)}: _*) } => sumExpr(args)
  case body => body
}

private def sumExpr(args1: Seq[Expr[Int]])(given QuoteContext): Expr[Int] = {
    def flatSumArgs(arg: Expr[Int]): Seq[Expr[Int]] = arg match {
      case '{ sum(${ExprSeq(subArgs)}: _*) } => subArgs.flatMap(flatSumArgs)
      case arg => Seq(arg)
    }
    val args2 = args1.flatMap(flatSumArgs)
    val staticSum: Int = args2.map {
      case Const(arg) => arg
      case _ => 0
    }.sum
    val dynamicSum: Seq[Expr[Int]] = args2.filter {
      case Const(_) => false
      case arg => true
    }
    dynamicSum.foldLeft(Expr(staticSum))((acc, arg) => '{ $acc + $arg })
}
