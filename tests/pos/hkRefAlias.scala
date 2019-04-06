class Bar
class X
class Y extends X

object Test {
  type G[X] = Bar { type R = X }

  implicitly[G[_] =:= (Bar { type R })]
  implicitly[G[_ >: Y <: X] =:= (Bar { type R >: Y <: X })]
}
