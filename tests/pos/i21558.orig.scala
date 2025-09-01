class Base
class A[T <: Float](val f: T) extends Base

def test() = {
  m1(new A(m2()));

}

def m1(x: Base) = {}
def m2(p: A[? <: Float] = new A(1.0f)): Int = 1
