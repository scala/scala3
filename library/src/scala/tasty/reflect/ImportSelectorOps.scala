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

trait ImportSelectorOps extends Core {

  val SimpleSelector: SimpleSelectorModule
  abstract class SimpleSelectorModule {
    def unapply(importSelector: ImportSelector)(implicit ctx: Context): Option[Id]
  }

  val RenameSelector: RenameSelectorModule
  abstract class RenameSelectorModule {
    def unapply(importSelector: ImportSelector)(implicit ctx: Context): Option[(Id, Id)]
  }

  val OmitSelector: OmitSelectorModule
  abstract class OmitSelectorModule {
    def unapply(importSelector: ImportSelector)(implicit ctx: Context): Option[Id]
  }

}
