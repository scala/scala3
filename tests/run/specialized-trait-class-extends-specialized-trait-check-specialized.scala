//> using options -language:experimental.specializedTraits
inline trait Foo[T: Specialized](x: T):
    def foo = x

class Bar extends Foo(10):
    def myMethod = "Hello I am a method"

@main def Test =
    val x = Bar()
    val traits = classOf[Bar].getInterfaces()
    assert(traits.exists(cl => cl.getName() == "Foo$sp$Int"))
