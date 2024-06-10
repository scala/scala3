// Like tests/neg/i15177.FakeEnum.min.scala
// But with an actual upper-bound requirement
// Which shouldn't be ignored as a part of overcoming the the cycle
trait Foo
trait X[T <: Foo] { trait Id }
object A extends X[B] // error: Type argument B does not conform to upper bound Foo
class B extends A.Id
