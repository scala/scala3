//> using options -language:experimental.inlineTraits
inline trait A:
    def bar = "A"

class E:
    def bar = "E"

class D extends E:
    class C extends A:
        def foo = D.super.bar // We shouldn't touch this super call when inlining A

@main def Test =
    val d = D()
    val c = d.C()
    assert(c.foo == "E")
