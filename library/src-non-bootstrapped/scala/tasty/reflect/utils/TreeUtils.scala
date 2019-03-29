package scala.tasty
package reflect.utils

trait TreeUtils {

  val reflect: Reflection
  import reflect._

  /** Bind the `rhs` to a `val` and use it in `body` */
  def let(rhs: Term)(bodyType: Type)(body: Ident => Term): Term =
    throw new Exception("non bootstrpped lib")

}
