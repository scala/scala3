type Foo
given myFoo1: (Foo { type X = Int }) = ???

type Bar = Foo { type X = Int }
given myFoo2: Bar = ???