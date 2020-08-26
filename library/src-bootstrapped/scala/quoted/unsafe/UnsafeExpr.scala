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
  def underlyingArgument[T](expr: Expr[T])(using qctx: QuoteContext): Expr[T] =
    expr.unseal.underlyingArgument.seal.asInstanceOf[Expr[T]]

  // TODO generalize for any function arity
  /** Allows inspection or transformation of the body of the expression of function.
   *  This body may have references to the arguments of the function which should be closed
   *  over if the expression will be spliced.
   *
   *  ```
   *  val f: Expr[T => R] = ...
   *  UnsafeExpr.open(f) { (body, close) =>
   *    val newParam: Expr[T] = ...
   *    ...
   *    close(body)(newParam) // body or part of the body
   *  }
   *  ```
   */
  def open[T1, R, X](f: Expr[T1 => R])(content: (Expr[R], [t] => Expr[t] => Expr[T1] => Expr[t]) => X)(using qctx: QuoteContext): X = {
    val (params, bodyExpr) = paramsAndBody[R](f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v: Expr[T1]) => bodyFn[t](e.unseal, params, List(v.unseal)).seal.asInstanceOf[Expr[t]])
  }

  def open[T1, T2, R, X](f: Expr[(T1, T2) => R])(content: (Expr[R], [t] => Expr[t] => (Expr[T1], Expr[T2]) => Expr[t]) => X)(using qctx: QuoteContext)(using DummyImplicit): X = {
    val (params, bodyExpr) = paramsAndBody[R](f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v1: Expr[T1], v2: Expr[T2]) => bodyFn[t](e.unseal, params, List(v1.unseal, v2.unseal)).seal.asInstanceOf[Expr[t]])
  }

  def open[T1, T2, T3, R, X](f: Expr[(T1, T2, T3) => R])(content: (Expr[R], [t] => Expr[t] => (Expr[T1], Expr[T2], Expr[T3]) => Expr[t]) => X)(using qctx: QuoteContext)(using DummyImplicit, DummyImplicit): X = {
    val (params, bodyExpr) = paramsAndBody[R](f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v1: Expr[T1], v2: Expr[T2], v3: Expr[T3]) => bodyFn[t](e.unseal, params, List(v1.unseal, v2.unseal, v3.unseal)).seal.asInstanceOf[Expr[t]])
  }

  private def paramsAndBody[R](using qctx: QuoteContext)(f: Expr[Any]): (List[qctx.tasty.ValDef], Expr[R]) = {
    import qctx.tasty._
    val Block(List(DefDef("$anonfun", Nil, List(params), _, Some(body))), Closure(Ident("$anonfun"), None)) = f.unseal.etaExpand
    (params, body.seal.asInstanceOf[Expr[R]])
  }

  private def bodyFn[t](using qctx: QuoteContext)(e: qctx.tasty.Term, params: List[qctx.tasty.ValDef], args: List[qctx.tasty.Term]): qctx.tasty.Term = {
    import qctx.tasty._
    val map = params.map(_.symbol).zip(args).toMap
    new TreeMap {
      override def transformTerm(tree: Term): Term =
        super.transformTerm(tree) match
          case tree: Ident => map.getOrElse(tree.symbol, tree)
          case tree => tree
    }.transformTerm(e)
  }
}
