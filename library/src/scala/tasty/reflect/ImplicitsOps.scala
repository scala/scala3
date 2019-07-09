package scala.tasty.reflect

trait ImplicitsOps extends Core {

  def searchImplicit(tpe: Type)(implicit ctx: Context): ImplicitSearchResult =
    kernel.searchImplicit(tpe)

  object IsImplicitSearchSuccess {
    def unapply(isr: ImplicitSearchResult)(implicit ctx: Context): Option[ImplicitSearchSuccess] =
      kernel.matchImplicitSearchSuccess(isr)
  }

  implicit class IsImplicitSearchSuccessAPI(self: ImplicitSearchSuccess) {
    def tree(implicit ctx: Context): Term = kernel.ImplicitSearchSuccess_tree(self)
  }

  object IsImplicitSearchFailure {
    def unapply(isr: ImplicitSearchResult)(implicit ctx: Context): Option[ImplicitSearchFailure] =
      kernel.matchImplicitSearchFailure(isr)
  }

  implicit class ImplicitSearchFailureAPI(self: ImplicitSearchFailure) {
    def explanation(implicit ctx: Context): String = kernel.ImplicitSearchFailure_explanation(self)
  }

  object IsDivergingImplicit {
    def unapply(isr: ImplicitSearchResult)(implicit ctx: Context): Option[DivergingImplicit] =
      kernel.matchDivergingImplicit(isr)
  }

  object IsNoMatchingImplicits {
    def unapply(isr: ImplicitSearchResult)(implicit ctx: Context): Option[NoMatchingImplicits] =
      kernel.matchNoMatchingImplicits(isr)
  }

  object IsAmbiguousImplicits {
    def unapply(isr: ImplicitSearchResult)(implicit ctx: Context): Option[AmbiguousImplicits] =
      kernel.matchAmbiguousImplicits(isr)
  }

}
