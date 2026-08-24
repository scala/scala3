package scala.util

import language.experimental.captureChecking

import scala.annotation.implicitNotFound
import scala.annotation.experimental

/** Type class relating a `FunctionN[..., R]` with an equivalent tupled function `Function1[TupleN[...], R]`
 *
 *  @tparam F a function type
 *  @tparam G a tupled function type (function of arity 1 receiving a tuple as argument)
 */
@implicitNotFound("${F} cannot be tupled as ${G}")
@experimental
sealed trait TupledFunction[F, G]:
  /** Converts a function `f` of type `F` to a tupled function of type `G`.
   *
   *  @param f the function to convert
   *  @return the tupled function equivalent to `f`
   */
  def tupled(f: F): G
  /** Converts a tupled function `g` of type `G` to a function of type `F`.
   *
   *  @param g the tupled function to convert
   *  @return the function equivalent to `g`
   */
  def untupled(g: G): F

@experimental
private[scala] object TupledFunction:
  /** Creates a `TupledFunction` instance from the given conversion functions.
   *
   *  @tparam F the function type to convert from
   *  @tparam G the tupled function type to convert to
   *  @param tupledImpl the function that converts from `F` to `G`
   *  @param untupledImpl the function that converts from `G` to `F`
   *  @return a `TupledFunction` instance that uses the provided conversion functions
   */
  def apply[F, G](tupledImpl: F => G, untupledImpl: G => F): TupledFunction[F, G]^{tupledImpl, untupledImpl} =
    new TupledFunction[F, G]:
      def tupled(f: F): G = tupledImpl(f)
      def untupled(g: G): F = untupledImpl(g)
