type Foo
given myFoo1 as (Foo { type X = Int }) = ???

type Bar = Foo { type X = Int }
given myFoo2 as Bar = ???