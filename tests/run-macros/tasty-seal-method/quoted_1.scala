import scala.quoted._

import scala.tasty._

object Asserts {

  inline def zeroLastArgs(x: => Int): Int =
    ${ zeroLastArgsImpl('x) }

  /** Replaces last argument list by 0s */
  def zeroLastArgsImpl(x: Expr[Int])(implicit reflect: Reflection): Expr[Int] = {
    import reflect._
    // For simplicity assumes that all parameters are Int and parameter lists have no more than 3 elements
    x.unseal.underlyingArgument match {
      case Apply(fn, args) =>
        fn.tpe.widen match {
          case Type.IsMethodType(_) =>
            args.size match {
              case 0 => fn.seal.cast[() => Int].apply()
              case 1 => fn.seal.cast[Int => Int].apply('{0})
              case 2 => fn.seal.cast[(Int, Int) => Int].apply('{0}, '{0})
              case 3 => fn.seal.cast[(Int, Int, Int) => Int].apply('{0}, '{0}, '{0})
            }
        }
      case _ => x
    }
  }

  inline def zeroAllArgs(x: => Int): Int =
    ${ zeroAllArgsImpl('x) }

  /** Replaces all argument list by 0s */
  def zeroAllArgsImpl(x: Expr[Int])(implicit reflect: Reflection): Expr[Int] = {
    import reflect._
    // For simplicity assumes that all parameters are Int and parameter lists have no more than 3 elements
    def rec(term: Term): Term = term match {
      case Apply(fn, args) =>
        val pre = rec(fn)
        args.size match {
          case 0 => pre.seal.cast[() => Any].apply().unseal
          case 1 => pre.seal.cast[Int => Any].apply('{0}).unseal
          case 2 => pre.seal.cast[(Int, Int) => Any].apply('{0}, '{0}).unseal
          case 3 => pre.seal.cast[(Int, Int, Int) => Any].apply('{0}, '{0}, '{0}).unseal
        }
      case _ => term
    }

    rec(x.unseal.underlyingArgument).seal.cast[Int]
  }

}
