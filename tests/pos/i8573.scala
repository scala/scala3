object Example1 {
  opaque type Foo[A] <: A = A
}

object Example2 {
  opaque type Foo[A] >: A = A
}