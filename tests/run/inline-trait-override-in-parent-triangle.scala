inline trait Foo:
    def foo = "Foo"

inline trait Bar extends Foo:
    override def foo = "Bar"

class C extends Bar, Foo
class D extends Foo, Bar

@main def Test = 
    val c = C()
    val d = D()
    assert(c.foo == "Bar")
    assert(d.foo == "Bar")
