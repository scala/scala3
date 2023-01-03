// Derived from the CI failure in compiling TreeUnpickler.scala
class Foo
class Bar extends Foo:
  type Self >: this.type <: Bar
  final def meth(): Self = this

class Test:
  def test(cond: Boolean, bar: Bar): Foo =
    val res = bar
    if cond then
      res.meth()
    else
      res
