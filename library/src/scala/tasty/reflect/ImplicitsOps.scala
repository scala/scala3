package scala.tasty.reflect

trait ImplicitsOps extends Core {

  def searchImplicit(tpe: Type)(given ctx: Context): ImplicitSearchResult =
    internal.searchImplicit(tpe)

  object IsImplicitSearchSuccess {
    def unapply(isr: ImplicitSearchResult)(given ctx: Context): Option[ImplicitSearchSuccess] =
      internal.matchImplicitSearchSuccess(isr)
  }

  implicit class IsImplicitSearchSuccessAPI(self: ImplicitSearchSuccess) {
    def tree(given ctx: Context): Term = internal.ImplicitSearchSuccess_tree(self)
  }

  object IsImplicitSearchFailure {
    def unapply(isr: ImplicitSearchResult)(given ctx: Context): Option[ImplicitSearchFailure] =
      internal.matchImplicitSearchFailure(isr)
  }

  implicit class ImplicitSearchFailureAPI(self: ImplicitSearchFailure) {
    def explanation(given ctx: Context): String = internal.ImplicitSearchFailure_explanation(self)
  }

  object IsDivergingImplicit {
    def unapply(isr: ImplicitSearchResult)(given ctx: Context): Option[DivergingImplicit] =
      internal.matchDivergingImplicit(isr)
  }

  object IsNoMatchingImplicits {
    def unapply(isr: ImplicitSearchResult)(given ctx: Context): Option[NoMatchingImplicits] =
      internal.matchNoMatchingImplicits(isr)
  }

  object IsAmbiguousImplicits {
    def unapply(isr: ImplicitSearchResult)(given ctx: Context): Option[AmbiguousImplicits] =
      internal.matchAmbiguousImplicits(isr)
  }

}
