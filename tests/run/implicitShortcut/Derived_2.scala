package implicitShortcut

class Derived extends Base[Int] {
  override def foo(x: Int): C |=> Int = 42
}