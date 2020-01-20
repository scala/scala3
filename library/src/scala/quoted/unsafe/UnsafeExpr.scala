package scala.quoted
package unsafe

object UnsafeExpr {

 /** Returns the undelying argument that was in the call before inlining.
  *
  *  ```
  *  inline foo(x: Int): Int = baz(x, x)
  *  foo(bar())
  *  ```
  *  is inlined as
  *  ```
  *  val x = bar()
  *  baz(x, x)
  *  ```
  *  in this case the undelying argument of `x` will be `bar()`.
  *
  *  Warning: Using the undelying argument directly in the expansion of a macro may
  *           change the parameter semantics as by-value parameter could be re-evaluated.
  */
  def underlyingArgument[T](expr: Expr[T])(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{given, _}
    expr.unseal.underlyingArgument.seal.asInstanceOf[Expr[T]]
  }


}
