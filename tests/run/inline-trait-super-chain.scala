inline trait A:
    def foo = "A"

inline trait B extends A:
    override def foo = super.foo

inline trait C extends B:
    override def foo = "B"

inline trait D extends C:
    override def foo = super.foo

inline trait E extends D:
    override def foo = super.foo

inline trait F extends E:
    override def foo = super.foo

class C1 extends F

@main def Test =
    val cl = C1()
    assert(cl.foo == "B")
