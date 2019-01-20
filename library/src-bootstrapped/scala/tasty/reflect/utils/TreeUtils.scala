package scala.tasty
package reflect.utils

import scala.quoted._

trait TreeUtils {

  val reflect: Reflection
  import reflect._

  def let(rhs: Term)(in: Term.Ident => Term): Term = {
    type T // TODO probably it is better to use the Sealed contruct rather than let the user create their own existential type
    implicit val rhsTpe: quoted.Type[T] = rhs.tpe.seal.asInstanceOf[quoted.Type[T]]
    val rhsExpr = rhs.seal[T]
    val expr = '{
      val x = ~rhsExpr
      ~in(('(x)).unseal.asInstanceOf[Term.Ident]).seal[Any]
    }
    expr.unseal
  }

}
