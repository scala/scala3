trait Foo:
  def foo(): Unit

class F1 extends Foo: // error
  def foo(): Unit

class F2 extends Foo: // error
  def foo(): Unit
  def foo(x: Int): Unit


trait Bar:
  def foo(): Unit
  def foo(x: Int): Unit

class B1 extends Bar: // error
  def foo(x: Int): Unit
