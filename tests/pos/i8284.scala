type Foo
given (Foo { type X = Int }) as myFoo1 = ???

type Bar = Foo { type X = Int }
given Bar as myFoo2 = ???