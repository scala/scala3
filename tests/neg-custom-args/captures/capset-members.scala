import caps.*

trait Abstract[X^]:
  type C >: X <: CapSet^
  // Don't test the return type using Unit, because it is a pure type.
  def boom(): AnyRef^{C}

class Concrete extends Abstract[CapSet^{}]:
  type C = CapSet^{}
  // TODO: Why do we get error without the return type here?
  def boom(): AnyRef = new Object

class Concrete2 extends Abstract[CapSet^{}]:
  type C = CapSet^{}
  def boom(): AnyRef^ = new Object // error

class Concrete3 extends Abstract[CapSet^{}]:
  def boom(): AnyRef = new Object

class Concrete4(a: AnyRef^) extends Abstract[CapSet^{a}]:
  type C = CapSet // error
  def boom(): AnyRef^{a} = a // error

class Concrete5(a: AnyRef^, b: AnyRef^) extends Abstract[CapSet^{a}]:
  type C = CapSet^{a}
  def boom(): AnyRef^{b} = b // error

class Concrete6(a: AnyRef^, b: AnyRef^) extends Abstract[CapSet^{a}]:
  def boom(): AnyRef^{b} = b // error
