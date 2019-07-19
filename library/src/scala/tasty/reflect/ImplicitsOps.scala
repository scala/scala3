package scala.tasty.reflect

trait ImplicitsOps extends Core {

  def searchImplicit(tpe: Type) given (ctx: Context): ImplicitSearchResult =
    kernel.searchImplicit(tpe)

  object IsImplicitSearchSuccess {
    def unapply(isr: ImplicitSearchResult) given (ctx: Context): Option[ImplicitSearchSuccess] =
      kernel.matchImplicitSearchSuccess(isr)
  }

  implicit class IsImplicitSearchSuccessAPI(self: ImplicitSearchSuccess) {
    def tree given (ctx: Context): Term = kernel.ImplicitSearchSuccess_tree(self)
  }

  object IsImplicitSearchFailure {
    def unapply(isr: ImplicitSearchResult) given (ctx: Context): Option[ImplicitSearchFailure] =
      kernel.matchImplicitSearchFailure(isr)
  }

  implicit class ImplicitSearchFailureAPI(self: ImplicitSearchFailure) {
    def explanation given (ctx: Context): String = kernel.ImplicitSearchFailure_explanation(self)
  }

  object IsDivergingImplicit {
    def unapply(isr: ImplicitSearchResult) given (ctx: Context): Option[DivergingImplicit] =
      kernel.matchDivergingImplicit(isr)
  }

  object IsNoMatchingImplicits {
    def unapply(isr: ImplicitSearchResult) given (ctx: Context): Option[NoMatchingImplicits] =
      kernel.matchNoMatchingImplicits(isr)
  }

  object IsAmbiguousImplicits {
    def unapply(isr: ImplicitSearchResult) given (ctx: Context): Option[AmbiguousImplicits] =
      kernel.matchAmbiguousImplicits(isr)
  }

}
