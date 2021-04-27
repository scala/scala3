trait Foo[F <: Foo[F]]
class Bar extends Foo[Bar]

object Q {    // error: recursion limit exceeded
  opaque type X <: Foo[X] = Bar // error: out of bounds // error
}