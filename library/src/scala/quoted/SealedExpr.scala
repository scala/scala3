package scala.quoted

/** A container for an `Expr[Tpe]` with its `Type[Tpe]` where `Tpe` might be unkown.
 *
 *  Example usage:
 *  ```
 *  def let[T](s: Sealed)(body: Expr[s.Tpe] => Expr[T]): Expr[T] = '{
 *    val x: ~s.tpe = ~s.expr
 *    ~body('(x))
 *  }
 *  ```
 */
final class Sealed private (e: Expr[_], t: Type[_]) {

  type Tpe

  val expr: Expr[Tpe] = e.asInstanceOf[Expr[Tpe]]

  val tpe: Type[Tpe] = t.asInstanceOf[Type[Tpe]]

}

object Sealed {
  def apply[T](expr: Expr[T])(implicit tpe: Type[T]): Sealed = new Sealed(expr, tpe)
}
