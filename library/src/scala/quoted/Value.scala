package scala.quoted

/** Constructors for expressions */
object Value:

  /** Creates an expression that will construct the value `x` */
  def apply[T](x: T)(using ToExpr[T])(using Quotes): Expr[T] =
    scala.Predef.summon[ToExpr[T]].apply(x)

  /** Get `Some` of a copy of the value if the expression contains a literal constant or constructor of `T`.
   *  Otherwise returns `None`.
   *
   *  Usage:
   *  ```
   *  case '{ ... ${expr @ Value(value)}: T ...} =>
   *    // expr: Expr[T]
   *    // value: T
   *  ```
   *
   *  To directly get the value of an expression `expr: Expr[T]` consider using `expr.value`/`expr.valueOrError` insead.
   */
  def unapply[T](x: Expr[T])(using FromExpr[T])(using Quotes): Option[T] =
    scala.Predef.summon[FromExpr[T]].unapply(x)

end Value
