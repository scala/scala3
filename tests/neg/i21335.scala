trait Foo:
  def bar(): Foo

trait Bar1 extends Foo:
  override def bar(): Bar1

class Z1 extends Bar1 // error

trait Bar2 extends Foo:
  def bar(): Bar2

class Z2 extends Bar2 // error
