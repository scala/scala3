import caps.*

trait Abstract[X^]:
  cap C >: X
  // Don't test the return type using Unit, because it is a pure type.
  def boom(): AnyRef^{C}

class Concrete extends Abstract[CapSet^{}]:
  cap C = {}
  // TODO: Why do we get error without the return type here?
  def boom(): AnyRef = new Object

class Concrete2 extends Abstract[CapSet^{}]:
  cap C = {}
  def boom(): AnyRef^ = new Object // error

class Concrete3 extends Abstract[CapSet^{}]:
  def boom(): AnyRef = new Object

class Concrete4(a: AnyRef^) extends Abstract[CapSet^{a}]:
  cap C = {} // error
  def boom(): AnyRef^{a} = a // error

class Concrete5(a: AnyRef^, b: AnyRef^) extends Abstract[CapSet^{a}]:
  cap C = a
  def boom(): AnyRef^{b} = b // error

class Concrete6(a: AnyRef^, b: AnyRef^) extends Abstract[CapSet^{a}]:
  def boom(): AnyRef^{b} = b // error