//> using options -language:experimental.specializedTraits
inline trait Foo[A: Specialized, B: Specialized, C: Specialized, D: Specialized, E: Specialized]:
    def foo = Thread.currentThread.getStackTrace()(1).getClassName()

inline trait Bar[A: Specialized, B, E]:
    def foo = Thread.currentThread.getStackTrace()(1).getClassName()

inline trait Baz[A: Specialized]:
    def foo = Thread.currentThread.getStackTrace()(1).getClassName()

@main def Test =
    val foo = new Foo[Any, AnyVal, Object, AnyRef, Nothing]() {}
    println(foo.foo)
    assert(foo.foo == "Foo$impl")

    val bar = new Bar[AnyVal, Int, String]() {}
    assert(bar.foo == "Bar$impl")

    val baz = new Baz[(Int, String)]() {}

    assert(baz.foo == "Baz$impl")
