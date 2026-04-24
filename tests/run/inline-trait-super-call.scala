inline trait A:
    def foo = "A"
    def bar = "bar"

inline trait B:
    def foo = "B"

class C extends A, B:
    override def foo = super.foo

class D extends A, B:
    override def foo = super[A].foo

class E extends A, B:
    override def foo = super[B].foo
    def baz = super[A].bar           // No override; this also needs to work

@main def Test: Unit = 
    val c = C()
    assert(c.foo == "B") 
    val d = D()
    assert(d.foo == "A") 
    val e = E()
    assert(e.foo == "B") 
