trait Foo[F <: Foo[F]]
class Bar extends Foo[Bar]

object Q {    // error: cyclic reference
  opaque type X <: Foo[X] = Bar // error: cyclic reference
}