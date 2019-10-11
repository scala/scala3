trait Foo { type X }
def f[T](given scala.quoted.TypeTag[T]) = ???
def g(m: Foo) = f[m.X] // error