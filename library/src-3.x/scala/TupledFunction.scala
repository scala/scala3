package scala

import scala.annotation.implicitNotFound

@implicitNotFound("${F} cannot be tupled as ${Args} => ${R}")
trait TupledFunction[F, Args <: Tuple, R] {
  def applyFunctionTo(f: F, args: Args): R
}

object TupledFunction {

  /** Apply this function to with each element of the tuple as a parameter */
  def (f: F) apply[F, Args <: Tuple, R](args: Args) given (tf: TupledFunction[F, Args, R]): R =
    tf.applyFunctionTo(f, args)

  /** Composes two instances of TupledFunctions in a new TupledFunctions, with this function applied last */
  def (f: F) compose[F, G, FArgs <: Tuple, GArgs <: Tuple, R](g: G) given TupledFunction[G, GArgs, FArgs], TupledFunction[F, FArgs, R]: GArgs => R = {
    x => f(g(x))
  }

  /** Composes two instances of TupledFunctions in a new TupledFunctions, with this function applied first */
  def (f: F) andThen[F, G, FArgs <: Tuple, GArgs <: Tuple, R](g: G) given TupledFunction[F, FArgs, GArgs], TupledFunction[G, GArgs, R]: FArgs => R = {
    x => g(f(x))
  }

}
