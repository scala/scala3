package scala.quoted
package matching

/** Lambda expression extractor */
object Lambda {

  /** `case Lambda(body)` matche a lambda and extract the body.
   *   As the body may (will) contain references to the paramter, `body` is a function that recieves those arguments as `Expr`.
   *   Once this function is applied the result will be the body of the lambda with all references to the parameters replaced.
   *   If `body` is of type `(T1, T2, ...) => R` then body will be of type `(Expr[T1], Expr[T2], ...) => Expr[R]`.
   *
   *   ```
   *   '{ (x: Int) => println(x) } match
   *     case Lambda(body) =>
   *       // where `body` is: (x: Expr[Int]) => '{ println($x) }
   *       body('{3}) // returns '{ println(3) }
   *   ```
   */
  def unapply[F, Args <: Tuple, Res, G](expr: Expr[F])(using qctx: QuoteContext, tf: TupledFunction[F, Args => Res], tg: TupledFunction[G, Tuple.Map[Args, Expr] => Expr[Res]]): Option[/*QuoteContext ?=>*/ G] = {
    import qctx.tasty.{_, given _ }
    qctx.tasty.internal.lambdaExtractor(expr.unseal).map { fn =>
      def f(args: Tuple.Map[Args, Expr]): Expr[Res] =
        fn(args.toArray.map(_.asInstanceOf[Expr[Any]].unseal).toList).seal.asInstanceOf[Expr[Res]]
      tg.untupled(f)
    }

  }

}
