package a

object Bar:
  given Foo.Local()
  def Bar = Foo.foo
