class Foo:
  def foo(x: => Foo) = bar(x: Foo)
  def bar(x: => Foo) = x
