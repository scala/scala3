// like tests/pos/i15177.scala
// but with B being higher kinded
// but without the actual cycle (like .without)
class X[T[_]] { type Id }
class A extends X[B]
class B[C]
