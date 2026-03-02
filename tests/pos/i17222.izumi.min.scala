class Foo:
  type In
  type Bar = { def go(in: In): Unit }
  type False = false

class Test:
  def t1: Unit = valueOf[Foo#False]
