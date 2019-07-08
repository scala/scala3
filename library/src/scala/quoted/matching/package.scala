package scala.quoted

package object matching {

  /** Find an implicit of type `T` in the current scope given by `qctx`.
   *  Return `Some` containing the expression of the implicit or
   * `None` if implicit resolution failed.
   *
   *  @tparam T type of the implicit parameter
   *  @param tpe quoted type of the implicit parameter
   *  @param qctx current context
   */
  def searchImplicitExpr[T] given (tpe: Type[T], qctx: QuoteContext): Option[Expr[T]] = {
    import qctx.tasty._
    searchImplicit(tpe.unseal.tpe) match {
      case IsImplicitSearchSuccess(iss) => Some(iss.tree.seal.asInstanceOf[Expr[T]])
      case IsImplicitSearchFailure(isf) => None
    }
  }

}
