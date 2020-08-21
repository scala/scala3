package scala.quoted

import scala.internal.tasty.CompilerInterface.quoteContextWithCompilerInterface

/** Lambda expression extractor */
object Lambda {

  /** `case Lambda(fn)` matches a lambda by lifting the function from `S => T` to `Expr[S] => Expr[T]`.
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
  def unapply[F, Args <: Tuple, Res, G](expr: Expr[F])(using qctx: QuoteContext, tf: TupledFunction[F, Args => Res], tg: TupledFunction[G, Tuple.Map[Args, Expr] => Expr[Res]], functionType: Type[F]): Option[/*QuoteContext ?=>*/ G] = {
    import qctx.tasty._
    val argTypes = functionType.unseal.tpe match
      case AppliedType(_, functionArguments) => functionArguments.init.asInstanceOf[List[Type]]
    val qctx2 = quoteContextWithCompilerInterface(qctx)
    qctx2.tasty.lambdaExtractor(expr.unseal, argTypes).map { fn =>
      def f(args: Tuple.Map[Args, Expr]): Expr[Res] =
        fn(args.toArray.toList.map(_.asInstanceOf[Expr[Any]].unseal)).seal.asInstanceOf[Expr[Res]]
      tg.untupled(f)
    }
  }

}
