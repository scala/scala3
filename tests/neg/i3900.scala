class Foo(transparent val i: Int) // error
case class Foo2(transparent val i: Int) // error
class Foo3(transparent val i: Int) extends AnyVal // error
trait Foo4(transparent val i: Int) // error
class Foo5() {
  def this(transparent x: Int) = this() // error
}
