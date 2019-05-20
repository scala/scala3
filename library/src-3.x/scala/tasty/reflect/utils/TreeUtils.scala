package scala.tasty
package reflect.utils

import scala.quoted._

trait TreeUtils {

  val reflect: Reflection
  import reflect._

  /** Bind the `rhs` to a `val` and use it in `body` */
  def let(rhs: Term)(body: Ident => Term): Term = {
    type T // TODO probably it is better to use the Sealed contruct rather than let the user create their own existential type
    implicit val rhsTpe: quoted.Type[T] = rhs.tpe.seal.asInstanceOf[quoted.Type[T]]
    val rhsExpr = rhs.seal.cast[T]
    val expr = '{
      val x = $rhsExpr
      ${
        val id = ('x).unseal.asInstanceOf[Ident]
        body(id).seal
      }
    }
    expr.unseal
  }

  /** Bind the given `terms` to names and use them in the `body` */
  def lets(terms: List[Term])(body: List[Term] => Term): Term = {
    def rec(xs: List[Term], acc: List[Term]): Term = xs match {
      case Nil => body(acc)
      case x :: xs => let(x) { (x: Term) => rec(xs, x :: acc) }
    }
    rec(terms, Nil)
  }
}
