trait Foo { type X }
def f[T](using s: quoted.Scope)(using s.Type[T]) = ???
def g(m: Foo) = f[m.X] // error