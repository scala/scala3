package scala.tasty
package reflect

trait ImportSelectorOps extends Core {

  given simpleSelectorOps: extension (self: SimpleSelector) {
    def selection(given ctx: Context): Id =
      internal.SimpleSelector_selection(self)
  }

  given (given Context): IsInstanceOf[SimpleSelector] = internal.isInstanceOfSimpleSelector

  object SimpleSelector
    def unapply(x: SimpleSelector)(given ctx: Context): Option[Id] = Some(x.selection)

  given renameSelectorOps: extension (self: RenameSelector) {
    def from(given ctx: Context): Id =
      internal.RenameSelector_from(self)

    def to(given ctx: Context): Id =
      internal.RenameSelector_to(self)
  }

  given (given Context): IsInstanceOf[RenameSelector] = internal.isInstanceOfRenameSelector

  object RenameSelector
    def unapply(x: RenameSelector)(given ctx: Context): Option[(Id, Id)] = Some((x.from, x.to))

  given omitSelectorOps: extension (self: OmitSelector) {
    def omitted(given ctx: Context): Id =
      internal.SimpleSelector_omitted(self)
  }

  given (given Context): IsInstanceOf[OmitSelector] = internal.isInstanceOfOmitSelector

  object OmitSelector
    def unapply(x: OmitSelector)(given ctx: Context): Option[Id] = Some(x.omitted)

}
