// Minimisation of tests/neg/i15177.FakeEnum.scala
trait X[T] { trait Id }
object A extends X[B]
class B extends A.Id
