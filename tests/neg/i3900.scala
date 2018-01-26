class Foo(inline val i: Int) // error
case class Foo2(inline val i: Int) // error
class Foo3(inline val i: Int) extends AnyVal // error
trait Foo4(inline val i: Int) // error
class Foo5() {
  def this(inline x: Int) = this() // error
}
