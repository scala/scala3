class A
class B

given A with {}
given B with {}

trait Foo
trait Bar

given Foo with {}
given Bar with {}

trait C
trait Baz[A]

given C with {}
given [A]: Baz[A] with {}