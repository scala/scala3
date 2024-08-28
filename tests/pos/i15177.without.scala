// like tests/pos/i15177.scala
// but without the actual cycle
class X[T] { trait Id }
class A extends X[B]
class B
