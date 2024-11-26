import caps.*

trait Abstract[X^]:
  type C >: X <: CapSet^
  def boom(): Unit^{C^}

class Concrete extends Abstract[CapSet^{}]:
  type C = CapSet^{}
  def boom() = ()

class Concrete2 extends Abstract[CapSet^{}]:
  type C = CapSet^{} & CapSet^{}
  def boom() = ()

class Concrete3 extends Abstract[CapSet^{}]:
  type C = CapSet^{} | CapSet^{}
  def boom() = ()

class Concrete4 extends Abstract[CapSet^]:
  type C = CapSet // error
  def boom() = ()
