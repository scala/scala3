def test = rule {
  foo(bar(baz))
}

def foo[I](r: I): Nothing = ???

def bar(i: Baz): i.Out = ???

sealed trait Baz:
  type Out = Nothing match { case Nothing => Nothing }

def baz: Baz = ???
