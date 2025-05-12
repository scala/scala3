import language.experimental.captureChecking
import caps.*

trait AbstractWrong:
  type C <: CapSet
  def f(): Unit^{C}

trait Abstract1:
  type C^
  def f(): Unit^{C}

// class Abstract2:
//   type C^
//   def f(): Unit^{C^}