import language.experimental.captureChecking
import caps.*

trait AbstractWrong:
  type C <: CapSet
  def f(): Unit^{C} // error

trait Abstract1:
  type C >: CapSet <: CapSet^ // error
  def f(): Unit^{C}

trait Abstract2:
  type C <: {cap}
  def f(): Unit^{C}