package pkg

trait Foo:
  def foo: this.type

final class Bar extends Foo:
  def foo: this.type = this
