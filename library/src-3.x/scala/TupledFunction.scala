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

}
