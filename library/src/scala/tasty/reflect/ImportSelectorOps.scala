package scala.tasty
package reflect

trait ImportSelectorOps extends Core {

  val SimpleSelector: SimpleSelectorExtractor
  abstract class SimpleSelectorExtractor {
    def unapply(importSelector: ImportSelector)(implicit ctx: Context): Option[Id]
  }

  val RenameSelector: RenameSelectorExtractor
  abstract class RenameSelectorExtractor {
    def unapply(importSelector: ImportSelector)(implicit ctx: Context): Option[(Id, Id)]
  }

  val OmitSelector: OmitSelectorExtractor
  abstract class OmitSelectorExtractor {
    def unapply(importSelector: ImportSelector)(implicit ctx: Context): Option[Id]
  }

}
