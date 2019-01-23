/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala.tasty
package reflect

trait IdOps extends Core {

  trait IdAPI {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position
    def name(implicit ctx: Context): String
  }
  implicit def IdDeco(id: Id): IdAPI

  val Id: IdModule
  abstract class IdModule {
    def unapply(id: Id): Option[String]
  }

}
