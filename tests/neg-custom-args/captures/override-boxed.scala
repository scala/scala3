
class A

def test(x: Any^{cap}) =
  abstract class Getter:
    def get(): A^{x}
  class PolyGetter[T <: A^{x}] extends Getter:
    override def get(): T = ??? // error
