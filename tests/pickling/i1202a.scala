package i1202a

class Test[T] {
  def testMethod: Unit =
    new Foo(this)
}
class Foo[T]() {
  def this(ct: Test[T]) = this()
}
