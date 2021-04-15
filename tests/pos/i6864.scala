class A
class B

given A()
given B()

trait Foo
trait Bar

given Foo()
given Bar()

trait C
trait Baz[A]

given C()
given [A]: Baz[A]()