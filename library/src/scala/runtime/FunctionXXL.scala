package scala.runtime

import language.experimental.captureChecking

/** A function with all parameters grouped in an array. */
trait FunctionXXL {

  /** Applies all parameters grouped in xs to this function.
   *
   *  @param xs the function arguments, packed into an immutable array of `Object`
   */
  def apply(xs: IArray[Object]): Object

  /** Returns a new `FunctionXXL` that applies this function and then `g` to the result.
   *
   *  Like `Function2..Function22.andThen`, this composes the (single-valued, here erased
   *  to `Object`) result of `apply` with `g`, yielding a function with the same shape.
   *
   *  @param g a function applied to the (erased) result of this
   *  @return  a new `FunctionXXL` `f` such that `f(xs) == g(apply(xs))`
   */
  def andThen(g: Object => Object): FunctionXXL^{this, g} = (xs: IArray[Object]) => g(apply(xs))

  override def toString() = "<functionXXL>"
}
