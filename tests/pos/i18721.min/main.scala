class Test:
  def t1(bar: Bar { val foo: Foo }): Any =
    meth(bar.foo)
