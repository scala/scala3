import language.experimental.captureChecking
import caps.*

class U

trait AbstractWrong:
  type C^ <: CapSet // no longer an error, the lower bound becomes CapSet now
  def f(): U^{C}

trait Abstract1:
  type C^ >: CapSet <: CapSet^
  def f(): U^{C}

trait Abstract2:
  type C^ <: {any}
  def f(): U^{C}