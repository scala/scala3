package pkg

trait Foo with
  def foo: this.type

final class Bar extends Foo with
  def foo: this.type = this
