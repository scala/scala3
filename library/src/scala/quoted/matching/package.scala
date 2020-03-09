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
  @deprecated("use scala.quoted.Expr.summon[T] instead", "0.23.0")
  def summonExpr[T](using tpe: Type[T])(using qctx: QuoteContext): Option[Expr[T]] =
    Expr.summon[T]

  @deprecated("use scala.quoted.Const instead", "0.23.0")
  val Const: quoted.Const.type = quoted.Const

  @deprecated("use scala.quoted.ConstSeq instead", "0.23.0")
  val ConstSeq: quoted.ConstSeq.type = quoted.ConstSeq

  @deprecated("use scala.quoted.ExprSeq instead", "0.23.0")
  val ExprSeq: quoted.ExprSeq.type = quoted.ExprSeq

  @deprecated("use scala.quoted.Lambda instead", "0.23.0")
  val Lambda: quoted.Lambda.type = quoted.Lambda

  @deprecated("use scala.quoted.Value instead", "0.23.0")
  val Value: quoted.Value.type = quoted.Value

  @deprecated("use scala.quoted.ValueOfExpr instead", "0.23.0")
  val ValueOfExpr: quoted.ValueOfExpr.type = quoted.ValueOfExpr

  @deprecated("use scala.quoted.ValueSeq instead", "0.23.0")
  val ValueSeq: quoted.ValueSeq.type = quoted.ValueSeq

}
