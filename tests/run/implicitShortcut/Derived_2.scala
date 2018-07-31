package implicitShortcut

class Derived extends Base[Int] {
  override def foo(x: Int): implicit C => Int = 42
}