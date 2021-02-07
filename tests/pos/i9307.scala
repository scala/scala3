import compiletime.uninitialized
class Foo:
  private var foo1: Int = uninitialized
  private var foo2: Array[Int] = uninitialized
  private[this] var foo3: Array[Int] = uninitialized
  private var foo4: Array[Object] = uninitialized
  private var foo5: Array[Array[Int]] = uninitialized
  private var foo6: List[Int] = uninitialized
