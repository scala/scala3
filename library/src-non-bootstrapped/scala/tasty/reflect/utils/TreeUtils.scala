/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tasty
package reflect.utils

trait TreeUtils {

  val reflect: Reflection
  import reflect._

  def let(rhs: Term)(in: Term.Ident => Term): Term = throw new Exception("non bootstrpped lib")

}
