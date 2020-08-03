trait Foo { type X }
def f[T](using scala.quoted.Staged[T]) = ???
def g(m: Foo) = f[m.X] // error