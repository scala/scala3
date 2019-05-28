package scala

import scala.annotation.implicitNotFound

/** Type class relating a `FunctionN[..., R]` with an equvalent tupled function `Function1[TupleN[...], R]`
 *
 *  @tparam F a function type
 *  @tparam G a tupled function type (function of arity 1 receiving a tuple as argument)
 */
@implicitNotFound("${F} cannot be tupled as ${G}")
trait TupledFunction[F, G] {
  def apply(f: F): G
}
