//> using options -language:experimental.specializedTraits
inline trait Foo[T: Specialized]:
    def foo = Thread.currentThread.getStackTrace()(1).getClassName()

def bar(xs: List[Foo[Int]]) = xs.head

@main def Test =
    val myList = List(new Foo[Int]() {}, new Foo[Int]() {})
    assert(bar(myList).foo == "Foo$impl$scala$Int")
