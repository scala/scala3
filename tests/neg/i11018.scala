trait Foo {
  def name: String
}
class Bar

trait CClass[+A](val a: A) {
  val b = a
}

trait CTrait[+A](val a: A) {
  val b = a
}
trait DTrait[+B] extends CTrait[B]
trait DClass[+B] extends CClass[B]

final class F1 extends DTrait[Foo] with CTrait[Bar](new Bar) // error: illegal parameter
final class F2 extends CTrait[Bar](new Bar) with DTrait[Foo] // error: illegal parameter
final class F3 extends DClass[Foo] with CClass[Bar](new Bar) // error: illegal parameter
final class F4 extends CClass[Bar](new Bar) with DClass[Foo] // error: illegal parameter

final class F5 extends DTrait[Foo] with CTrait[Foo & Bar](new Bar with Foo { def name = "hello"}) // ok
