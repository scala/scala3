class Foo {
  protected[this] def foo = 1
  protected def foo2 = 1
  val foo3 = 1

}

class Bar extends Foo {
  // illegal combination of modifiers: `private` and `override`
  override private[this] def foo = 2 // error
  override private[this] def foo2 = 2 // error
  override private val foo3 = 2 // error
}
