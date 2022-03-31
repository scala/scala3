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

final class F1 // error: illegal inheritance
  extends DTrait[Foo]
  with CTrait[Bar](new Bar) // error: illegal parameter
final class F2 // error: illegal inheritance
  extends CTrait[Bar](new Bar) // error: illegal parameter
  with DTrait[Foo]
final class F3 // error: illegal inheritance
  extends DClass[Foo]
  with CClass[Bar](new Bar) // error: illegal parameter
final class F4 // error: illegal inheritance
  extends CClass[Bar](new Bar) // error: illegal parameter
  with DClass[Foo]

final class F5 // error: illegal inheritance
  extends DTrait[Foo]
  with CTrait[Foo & Bar](new Bar with Foo { def name = "hello"})
