trait Foo { type Num <: Int }
given derived[A](using foo: Foo): Any = derivedImpl(foo)  // error
def derivedImpl(foo: Foo)(using bar: foo.Num =:= Int): Any = ???
