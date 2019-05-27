package scala

import scala.annotation.implicitNotFound

/** Type class relating a `FunctionN[..., R]` with an equvalent tupled function `Function1[TupleN[...], R]`
 *
 *  @tparam F a function type
 *  @tparam Args a tuple type with the same types as the function arguments of F
 *  @tparam R the return type of F
 */
@implicitNotFound("${F} cannot be tupled as ${Args} => ${R}")
trait TupledFunction[F, Args <: Tuple, R] {
  def applyFunctionTo(f: F, args: Args): R
}

/** Module of TupledFunction containing methods for auto function tupling
 *
 *  Usage
 *  ```
 *  val t2: (Int, Int) = ???
 *  val t3: (Int, Int, Int) = ???
 *  val f1: (Int, Int) => (Int, Int, Int) = ???
 *  val f2: (Int, Int, Int) => (Int, Int) = ???
 *
 *  import TupledFunction._
 *  f1(t2)
 *  f2(t3)
 *  val f3: (Int, Int) => (Int, Int) = f1.andThen(f2)
 *  val f4: (Int, Int, Int) => (Int, Int, Int) = f1.compose(f2)
 *  ```
 */
object TupledFunction {

  /** Apply this function to with each element of the tuple as a parameter
   *
   *  @tparam F the function type
   *  @tparam Args the tuple type with the same types as the function arguments of F
   *  @tparam R the return type of F
   */
  def (f: F) apply[F, Args <: Tuple, R](args: Args) given (tf: TupledFunction[F, Args, R]): R =
    tf.applyFunctionTo(f, args)

  /** Composes two instances of TupledFunctions in a new TupledFunctions, with this function applied last
   *
   *  @tparam F a function type
   *  @tparam G a function type
   *  @tparam FArgs the tuple type with the same types as the function arguments of F and return type of G
   *  @tparam GArgs the tuple type with the same types as the function arguments of G
   *  @tparam R the return type of F
   */
  def (f: F) compose[F, G, FArgs <: Tuple, GArgs <: Tuple, R](g: G) given TupledFunction[G, GArgs, FArgs], TupledFunction[F, FArgs, R]: GArgs => R = {
    x => f(g(x))
  }

  /** Composes two instances of TupledFunctions in a new TupledFunctions, with this function applied first
   *
   *  @tparam F a function type
   *  @tparam G a function type
   *  @tparam FArgs the tuple type with the same types as the function arguments of F
   *  @tparam GArgs the tuple type with the same types as the function arguments of G and return type of F
   *  @tparam R the return type of G
   */
  def (f: F) andThen[F, G, FArgs <: Tuple, GArgs <: Tuple, R](g: G) given TupledFunction[F, FArgs, GArgs], TupledFunction[G, GArgs, R]: FArgs => R = {
    x => g(f(x))
  }

}
