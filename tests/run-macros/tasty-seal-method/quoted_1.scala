import scala.quoted._

object Asserts {

  inline def zeroLastArgs(x: => Int): Int =
    ${ zeroLastArgsImpl('x) }

  /** Replaces last argument list by 0s */
  def zeroLastArgsImpl(x: Expr[Int])(using qctx: QuoteContext) : Expr[Int] = {
    import qctx.reflect._
    // For simplicity assumes that all parameters are Int and parameter lists have no more than 3 elements
    x.asReflectTree.underlyingArgument match {
      case Apply(fn, args) =>
        fn.tpe.widen match {
          case _: MethodType =>
            args.size match {
              case 0 => Expr.betaReduce('{ ${fn.etaExpand.asExprOf[() => Int]}() })
              case 1 => Expr.betaReduce('{ ${fn.etaExpand.asExprOf[Int => Int]}(0) })
              case 2 => Expr.betaReduce('{ ${fn.etaExpand.asExprOf[(Int, Int) => Int]}(0, 0) })
              case 3 => Expr.betaReduce('{ ${fn.etaExpand.asExprOf[(Int, Int, Int) => Int]}(0, 0, 0) })
            }
        }
      case _ => x
    }
  }

  inline def zeroAllArgs(x: => Int): Int =
    ${ zeroAllArgsImpl('x) }

  /** Replaces all argument list by 0s */
  def zeroAllArgsImpl(x: Expr[Int])(using qctx: QuoteContext) : Expr[Int] = {
    import qctx.reflect._
    // For simplicity assumes that all parameters are Int and parameter lists have no more than 3 elements
    def rec(term: Term): Term = term match {
      case Apply(fn, args) =>
        val pre = rec(fn)
        args.size match {
          case 0 => Expr.betaReduce('{ ${pre.etaExpand.asExprOf[() => Any]}() }).asReflectTree
          case 1 => Expr.betaReduce('{ ${pre.etaExpand.asExprOf[Int => Any]}(0) }).asReflectTree
          case 2 => Expr.betaReduce('{ ${pre.etaExpand.asExprOf[(Int, Int) => Any]}(0, 0) }).asReflectTree
          case 3 => Expr.betaReduce('{ ${pre.etaExpand.asExprOf[(Int, Int, Int) => Any]}(0, 0, 0) }).asReflectTree
        }
      case _ => term
    }

    rec(x.asReflectTree.underlyingArgument).asExprOf[Int]
  }

}
