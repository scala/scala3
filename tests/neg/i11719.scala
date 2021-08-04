class Base
class Sub extends Base

trait A[+T] {
  def get: T = ???
}

trait B extends A[Base] {
  override def get: Base = new Base
}

class C extends B with A[Sub] // error: method get in trait B is not a legal implementation