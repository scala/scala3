class A

def test(x: {*} Any) =
  abstract class Getter:
    def get(): {x} A
  class PolyGetter[T <: {x} A] extends Getter:
    override def get(): T = ??? // error
