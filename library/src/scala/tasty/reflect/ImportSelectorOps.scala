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
