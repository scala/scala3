//> using options -language:experimental.specializedTraits
package package1 {
    object Outer:
        object Inner:
            inline trait A[T: Specialized]:
                def foo(x: T) = "Package 1!"
                def bar = Thread.currentThread.getStackTrace()(1).getClassName()
    class B extends Outer.Inner.A[Int]
    class C extends Outer.Inner.A[String]
}

@main def Test = 
    val b = package1.B()
    val c = package1.C()
    assert(b.foo(10) == "Package 1!")
    assert(c.foo("Hello World") == "Package 1!")
    
    val d = new package1.Outer.Inner.A[Int]() {}
    assert(d.bar == "package1.Outer$Inner$$A$impl$scala$Int")

