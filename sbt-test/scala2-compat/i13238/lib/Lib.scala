package lib

trait Bar
class Foo {
  def foo(out: Foo)(implicit bar: Bar): out.type = out
}
