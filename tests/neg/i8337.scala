trait Foo[F <: Foo[F]]
class Bar extends Foo[Bar]

object Q {    // error: object Q cannot be instantiated since it has a member X with possibly conflicting bounds Any <: ... <: Foo[Q.X]
  opaque type X <: Foo[X] = Bar
}
