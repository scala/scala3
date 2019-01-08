package scala.tasty
package reflect.utils

trait TreeUtils {

  val reflect: Reflection
  import reflect._

  def let(rhs: Term)(in: Term.Ident => Term): Term = throw new Exception("non bootstrpped lib")

}
