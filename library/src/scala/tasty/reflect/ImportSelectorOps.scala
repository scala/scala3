package scala.tasty
package reflect

trait ImportSelectorOps extends Core {

  given SimpleSelectorOps: (self: SimpleSelector) {
    def selection(given ctx: Context): Id =
      internal.SimpleSelector_selection(self)
  }

  object SimpleSelector {
    def unapply(importSelector: ImportSelector)(given ctx: Context): Option[Id] =
      internal.matchSimpleSelector(importSelector).map(_.selection)
  }

  given RenameSelectorOps: (self: RenameSelector) {
    def from(given ctx: Context): Id =
      internal.RenameSelector_from(self)

    def to(given ctx: Context): Id =
      internal.RenameSelector_to(self)
  }

  object RenameSelector {
    def unapply(importSelector: ImportSelector)(given ctx: Context): Option[(Id, Id)] =
      internal.matchRenameSelector(importSelector).map(x => (x.from, x.to))
  }

  given OmitSelectorOps: (self: OmitSelector) {
    def omitted(given ctx: Context): Id =
      internal.SimpleSelector_omitted(self)
  }

  object OmitSelector {
    def unapply(importSelector: ImportSelector)(given ctx: Context): Option[Id] =
      internal.matchOmitSelector(importSelector).map(_.omitted)
  }

}
