abstract class Foo {
  def bar: Unit = {}
  def baz
  def boo(i: Int, l: Long)
  def boz(i: Int, l: Long): Unit = {}
  def this(i: Int) = { this() } // Don't complain here!
  def foz: Unit               // Don't complain here!
}
