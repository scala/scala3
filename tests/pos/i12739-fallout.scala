// This is a minimisation of the fallout that the original fix caused on Shapeless 3.

type Foo = { type Bar }

extension (foo: Foo)
  def toBar(): foo.Bar = ???

def test(foo: Foo): foo.Bar = foo.toBar()
