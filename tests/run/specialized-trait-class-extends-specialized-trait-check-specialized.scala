//> using options -language:experimental.specializedTraits
inline trait Foo[T: Specialized](x: T):
    def foo = x

class Bar extends Foo[Int](10):
    def myMethod = "Hello I am a method"

class Bar2 extends Foo(10):
    def myMethod = "Hello I am a method"

@main def Test =
    val y = new Foo(10) {}
    val x = Bar()
    val traits = classOf[Bar].getInterfaces()
    
    assert(traits.exists(cl => cl.getName() == "Foo$sp$scala$Int"))

    val traits2 = classOf[Bar2].getInterfaces()
    assert(traits2.exists(cl => cl.getName() == "Foo$sp$scala$Int"))
