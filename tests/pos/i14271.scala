// https://github.com/scala/scala3/issues/14271
class Bound[T]
class MyClass[T <: Bound[T]]

class Container[V] {
  def doSth(): V = ???
}

def bug() = {
  val m = new Container[MyClass[_]]
  if (true) {
    m.doSth()
  }
}
