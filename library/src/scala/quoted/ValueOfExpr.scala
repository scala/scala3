package scala.quoted

/** A typeclass for types that can be turned from a `quoted.Expr[T]` to a `T` */
trait ValueOfExpr[T] {

  /** Return the value of the expression.
   *
   *  Returns `None` if the expression does not contain a value or contains side effects.
   *  Otherwise returns the `Some` of the value.
   */
  def apply(x: Expr[T]) given QuoteContext: Option[T]

}

object ValueOfExpr {

  implicit val ValueOfExpr_Unit_delegate: ValueOfExpr[Unit] = new PrimitiveValueOfExpr
  implicit val ValueOfExpr_Boolean_delegate: ValueOfExpr[Boolean] = new PrimitiveValueOfExpr
  implicit val ValueOfExpr_Byte_delegate: ValueOfExpr[Byte] = new PrimitiveValueOfExpr
  implicit val ValueOfExpr_Short_delegate: ValueOfExpr[Short] = new PrimitiveValueOfExpr
  implicit val ValueOfExpr_Int_delegate: ValueOfExpr[Int] = new PrimitiveValueOfExpr
  implicit val ValueOfExpr_Long_delegate: ValueOfExpr[Long] = new PrimitiveValueOfExpr
  implicit val ValueOfExpr_Float_delegate: ValueOfExpr[Float] = new PrimitiveValueOfExpr
  implicit val ValueOfExpr_Double_delegate: ValueOfExpr[Double] = new PrimitiveValueOfExpr
  implicit val ValueOfExpr_Char_delegate: ValueOfExpr[Char] = new PrimitiveValueOfExpr
  implicit val ValueOfExpr_String_delegate: ValueOfExpr[String] = new PrimitiveValueOfExpr

  private class PrimitiveValueOfExpr[T <: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String] extends ValueOfExpr[T] {
    /** Lift a quoted primitive value `'{ n }` into `n` */
    def apply(x: Expr[T]) given QuoteContext: Option[T] = matching.Const.unapply(x)
  }

}
