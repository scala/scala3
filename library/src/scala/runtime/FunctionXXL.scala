package scala.runtime

import language.experimental.captureChecking

/** A function with all parameters grouped in an array. */
trait FunctionXXL {

  /** Applies all parameters grouped in xs to this function.
   *
   *  @param xs the function arguments, packed into an immutable array of `Object`
   *  @return the result of applying this function to the given arguments
   */
  def apply(xs: IArray[Object]): Object

  /** Returns the string `"<functionXXL>"`, mirroring the `"<functionN>"` rendering of the `Function0` to `Function22` traits. */
  override def toString() = "<functionXXL>"
}
