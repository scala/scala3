/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala.tasty
package reflect.utils

trait TreeUtils {

  val reflect: Reflection
  import reflect._

  def let(rhs: Term)(in: Term.Ident => Term): Term = throw new Exception("non bootstrpped lib")

}
