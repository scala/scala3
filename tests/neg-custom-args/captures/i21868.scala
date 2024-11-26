import caps.*

trait AbstractWrong:
  type C <: CapSet
  def f(): Unit^{C^} // error

trait Abstract1:
  type C^
  def f(): Unit^{C^}

class Abstract2:
  type C >: CapSet <: CapSet^
  def f(): Unit^{C^}