import caps.*

trait Abstract[cap X]:
  cap type C >: {X}
  // Don't test the return type using Unit, because it is a pure type.
  def boom(): AnyRef^{C}

class Concrete extends Abstract[{}]:
  cap type C = {}
  // TODO: Why do we get error without the return type here?
  def boom(): AnyRef = new Object

class Concrete2 extends Abstract[{}]:
  cap type C = {}
  def boom(): AnyRef^ = new Object // error

class Concrete3 extends Abstract[{}]:
  def boom(): AnyRef = new Object

class Concrete4(a: AnyRef^) extends Abstract[{a}]:
  cap type C = {} // error
  def boom(): AnyRef^{a} = a // error

class Concrete5(a: AnyRef^, b: AnyRef^) extends Abstract[{a}]:
  cap type C = {a}
  def boom(): AnyRef^{b} = b // error

class Concrete6(a: AnyRef^, b: AnyRef^) extends Abstract[{a}]:
  def boom(): AnyRef^{b} = b // error