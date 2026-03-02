// Like tests/neg/i15177.FakeEnum.min.scala
// With an actual upper-bound requirement
// But that is satisfied on class B
trait Foo
trait X[T <: Foo] { trait Id }
object A extends X[B]
class B extends A.Id with Foo
