import scala.quoted._

object Asserts {

  inline def zeroLastArgs(x: => Int): Int =
    ${ zeroLastArgsImpl('x) }

  /** Replaces last argument list by 0s */
  def zeroLastArgsImpl(using s: Scope)(x: s.Expr[Int]): s.Expr[Int] = {
    import s.tasty._
    given g0 as s.Type[() => Int] = '[() => Int] // FIXME remove
    given g1 as s.Type[Int => Int] = '[Int => Int] // FIXME remove
    given g2 as s.Type[(Int, Int) => Int] = '[(Int, Int) => Int] // FIXME remove
    given g3 as s.Type[(Int, Int, Int) => Int] = '[(Int, Int, Int) => Int] // FIXME remove
    // For simplicity assumes that all parameters are Int and parameter lists have no more than 3 elements
    x.underlyingArgument match {
      case Apply(fn, args) =>
        fn.tpe.widen match {
          case _: MethodType =>
            args.size match {
              case 0 => Expr.betaReduce('{ ${fn.etaExpand.seal.cast[() => Int]}() })
              case 1 => Expr.betaReduce('{ ${fn.etaExpand.seal.cast[Int => Int]}(0) })
              case 2 => Expr.betaReduce('{ ${fn.etaExpand.seal.cast[(Int, Int) => Int]}(0, 0) })
              case 3 => Expr.betaReduce('{ ${fn.etaExpand.seal.cast[(Int, Int, Int) => Int]}(0, 0, 0) })
            }
        }
      case _ => x
    }
  }

  inline def zeroAllArgs(x: => Int): Int =
    ${ zeroAllArgsImpl('x) }

  /** Replaces all argument list by 0s */
  def zeroAllArgsImpl(using s: Scope)(x: s.Expr[Int]): s.Expr[Int] = {
    import s.tasty._
    given g0 as s.Type[() => Any] = '[() => Any] // FIXME remove
    given g1 as s.Type[Int => Any] = '[Int => Any] // FIXME remove
    given g2 as s.Type[(Int, Int) => Any] = '[(Int, Int) => Any] // FIXME remove
    given g3 as s.Type[(Int, Int, Int) => Any] = '[(Int, Int, Int) => Any] // FIXME remove
    // For simplicity assumes that all parameters are Int and parameter lists have no more than 3 elements
    def rec(term: Term): Term = term match {
      case Apply(fn, args) =>
        val pre = rec(fn)
        args.size match {
          case 0 => Expr.betaReduce('{ ${pre.etaExpand.seal.cast[() => Any]}() })
          case 1 => Expr.betaReduce('{ ${pre.etaExpand.seal.cast[Int => Any]}(0) })
          case 2 => Expr.betaReduce('{ ${pre.etaExpand.seal.cast[(Int, Int) => Any]}(0, 0) })
          case 3 => Expr.betaReduce('{ ${pre.etaExpand.seal.cast[(Int, Int, Int) => Any]}(0, 0, 0) })
        }
      case _ => term
    }

    rec(x.underlyingArgument).seal.cast[Int]
  }

}
