package core

opaque type Foo[T] <: Int = Int
type LeakingFoo[T] = Foo[T]
