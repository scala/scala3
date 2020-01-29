package scala.tasty.reflect

trait ImplicitsOps extends Core {

  def searchImplicit(tpe: Type)(given ctx: Context): ImplicitSearchResult =
    internal.searchImplicit(tpe)

  given (given Context): IsInstanceOf[ImplicitSearchSuccess] = internal.isInstanceOfImplicitSearchSuccess

  given successOps: extension (self: ImplicitSearchSuccess) {
    def tree(given ctx: Context): Term = internal.ImplicitSearchSuccess_tree(self)
  }

  given (given Context): IsInstanceOf[ImplicitSearchFailure] = internal.isInstanceOfImplicitSearchFailure

  given failureOps: extension (self: ImplicitSearchFailure) {
    def explanation(given ctx: Context): String = internal.ImplicitSearchFailure_explanation(self)
  }

  given (given Context): IsInstanceOf[DivergingImplicit] = internal.isInstanceOfDivergingImplicit

  given (given Context): IsInstanceOf[NoMatchingImplicits] = internal.isInstanceOfNoMatchingImplicits

  given (given Context): IsInstanceOf[AmbiguousImplicits] = internal.isInstanceOfAmbiguousImplicits

}
