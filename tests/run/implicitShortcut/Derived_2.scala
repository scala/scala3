package implicitShortcut

class Derived extends Base[Int] {
  override def foo(x: Int): given C => Int = 42
}