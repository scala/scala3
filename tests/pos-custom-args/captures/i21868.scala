import language.experimental.captureChecking
import caps.*

trait AbstractWrong:
  type C^ <: CapSet // no longer an error, the lower bound becomes CapSet now
  def f(): Unit^{C}

trait Abstract1:
  type C^ >: CapSet <: CapSet^
  def f(): Unit^{C}

trait Abstract2:
  type C^ <: {cap}
  def f(): Unit^{C}