// Like tests/neg/i15177.FakeEnum.min.scala
// With an actual upper-bound requirement
// But that is satisfied on trait Id
trait Foo
trait X[T <: Foo] { trait Id extends Foo }
object A extends X[B]
class B extends A.Id
