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
  def unapply[F, Args <: Tuple, Res, G](using s: Scope)(expr: s.Expr[F])(using tf: TupledFunction[F, Args => Res], tg: TupledFunction[G, Tuple.Map[Args, s.Expr] => s.Expr[Res]], functionType: s.Type[F]): Option[/*Scope ?=>*/ G] = {
    import s.tasty._
    val argTypes = functionType.tpe match
      case AppliedType(_, functionArguments) => functionArguments.init.asInstanceOf[List[Type]]
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.lambdaExtractor(expr, argTypes).map { fn =>
      def f(args: Tuple.Map[Args, s.Expr]): s.Expr[Res] =
        fn(args.toArray.toList.map(_.asInstanceOf[s.Expr[Any]])).seal.asInstanceOf[s.Expr[Res]]
      tg.untupled(f) // TODO remove asInstanceOf
    }
  }

}
