trait Foo[A]
trait Bar[A] extends Foo[A]
trait Baz[A] extends Bar[A]

trait FooLaws[A](using Foo[A])
trait BarLaws[A](using Bar[A]) extends FooLaws[A]
trait BazLaws[A](using Baz[A]) extends BarLaws[A]

def instance[A](using Foo[A]): BazLaws[A] =
  new BazLaws[A] {}  // error // error

