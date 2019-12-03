package scala.tasty.reflect

trait ImplicitsOps extends Core {

  def searchImplicit(tpe: Type)(given ctx: Context): ImplicitSearchResult =
    internal.searchImplicit(tpe)

  given (given Context): IsInstanceOf[ImplicitSearchSuccess] = internal.isInstanceOfImplicitSearchSuccess

  object IsImplicitSearchSuccess
    @deprecated("Use _: ImplicitSearchSuccess", "")
    def unapply(isr: ImplicitSearchSuccess)(given ctx: Context): Option[ImplicitSearchSuccess] = Some(isr)

  given successOps: extension (self: ImplicitSearchSuccess) {
    def tree(given ctx: Context): Term = internal.ImplicitSearchSuccess_tree(self)
  }

  given (given Context): IsInstanceOf[ImplicitSearchFailure] = internal.isInstanceOfImplicitSearchFailure

  object IsImplicitSearchFailure
    @deprecated("Use _: ImplicitSearchFailure", "")
    def unapply(isr: ImplicitSearchFailure)(given ctx: Context): Option[ImplicitSearchFailure] = Some(isr)

  given failureOps: extension (self: ImplicitSearchFailure) {
    def explanation(given ctx: Context): String = internal.ImplicitSearchFailure_explanation(self)
  }

  given (given Context): IsInstanceOf[DivergingImplicit] = internal.isInstanceOfDivergingImplicit

  object IsDivergingImplicit
    @deprecated("Use _: DivergingImplicit", "")
    def unapply(isr: DivergingImplicit)(given ctx: Context): Option[DivergingImplicit] = Some(isr)

  given (given Context): IsInstanceOf[NoMatchingImplicits] = internal.isInstanceOfNoMatchingImplicits

  object IsNoMatchingImplicits
    @deprecated("Use _: NoMatchingImplicits", "")
    def unapply(isr: NoMatchingImplicits)(given ctx: Context): Option[NoMatchingImplicits] = Some(isr)

  given (given Context): IsInstanceOf[AmbiguousImplicits] = internal.isInstanceOfAmbiguousImplicits

  object IsAmbiguousImplicits
    @deprecated("Use _: AmbiguousImplicits", "")
    def unapply(isr: AmbiguousImplicits)(given ctx: Context): Option[AmbiguousImplicits] = Some(isr)

}
