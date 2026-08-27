package scala.runtime

import language.experimental.captureChecking

/** A function with all parameters grouped in an array. */
trait FunctionXXL {

  /** Applies all parameters grouped in xs to this function.
   *
   *  @param xs the function arguments, packed into an immutable array of `Object`
   */
  def apply(xs: IArray[Object]): Object

  override def toString() = "<functionXXL>"
}
