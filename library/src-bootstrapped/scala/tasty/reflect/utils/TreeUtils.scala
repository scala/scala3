package scala.tasty
package reflect.utils

import scala.quoted._

trait TreeUtils {

  val reflect: Reflection
  import reflect._

  /** Bind the `rhs` to a `val` and use it in `body` */
  def let(rhs: Term)(bodyType: Type)(body: Term.Ident => Term): Term = {
    // Recover all lost type information
    type T // TODO probably it is better to use the Sealed contruct rather than let the user create their own existential type
    type U // TODO probably it is better to use the Sealed contruct rather than let the user create their own existential type
    implicit val bodyTpe: quoted.Type[U] = bodyType.seal.asInstanceOf[quoted.Type[U]]
    implicit val rhsTpe: quoted.Type[T] = rhs.tpe.seal.asInstanceOf[quoted.Type[T]]
    val rhsExpr = rhs.seal[T]
    let[T, U](rhsExpr)(x => body(x.unseal.asInstanceOf[Term.Ident]).seal[U]).unseal
  }

  /**  */
  private def let[T: quoted.Type, U: quoted.Type](rhs: Expr[T])(in: Expr[T] => Expr[U]): Expr[U] = '{
    val x = ~rhs
    ~in('(x))
  }

}
