import scala.quoted._

object Asserts {

  inline def zeroLastArgs(x: => Int): Int =
    ${ zeroLastArgsImpl('x) }

  /** Replaces last argument list by 0s */
  def zeroLastArgsImpl(x: Expr[Int])(given qctx: QuoteContext): Expr[Int] = {
    import qctx.tasty.{_, given}
    // For simplicity assumes that all parameters are Int and parameter lists have no more than 3 elements
    x.unseal.underlyingArgument match {
      case Apply(fn, args) =>
        fn.tpe.widen match {
          case IsMethodType(_) =>
            args.size match {
              case 0 => Expr.betaReduce(fn.seal.cast[() => Int])()
              case 1 => Expr.betaReduce(fn.seal.cast[Int => Int])('{0})
              case 2 => Expr.betaReduce(fn.seal.cast[(Int, Int) => Int])('{0}, '{0})
              case 3 => Expr.betaReduce(fn.seal.cast[(Int, Int, Int) => Int])('{0}, '{0}, '{0})
            }
        }
      case _ => x
    }
  }

  inline def zeroAllArgs(x: => Int): Int =
    ${ zeroAllArgsImpl('x) }

  /** Replaces all argument list by 0s */
  def zeroAllArgsImpl(x: Expr[Int])(given qctx: QuoteContext): Expr[Int] = {
    import qctx.tasty.{_, given}
    // For simplicity assumes that all parameters are Int and parameter lists have no more than 3 elements
    def rec(term: Term): Term = term match {
      case Apply(fn, args) =>
        val pre = rec(fn)
        args.size match {
          case 0 => Expr.betaReduce(pre.seal.cast[() => Any])().unseal
          case 1 => Expr.betaReduce(pre.seal.cast[Int => Any])('{0}).unseal
          case 2 => Expr.betaReduce(pre.seal.cast[(Int, Int) => Any])('{0}, '{0}).unseal
          case 3 => Expr.betaReduce(pre.seal.cast[(Int, Int, Int) => Any])('{0}, '{0}, '{0}).unseal
        }
      case _ => term
    }

    rec(x.unseal.underlyingArgument).seal.cast[Int]
  }

}
