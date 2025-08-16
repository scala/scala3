package scala.runtime

import language.experimental.captureChecking

/** A function with all parameters grouped in an array. */
trait FunctionXXL {

  /** Apply all parameters grouped in xs to this function. */
  def apply(xs: IArray[Object]): Object

  override def toString() = "<functionXXL>"
}
