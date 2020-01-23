trait Foo { type X }
def f[T] with scala.quoted.Type[T] = ???
def g(m: Foo) = f[m.X] // error