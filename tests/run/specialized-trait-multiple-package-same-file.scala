//> using options -language:experimental.specializedTraits

package package1 {
    inline trait A[T: Specialized]:
        def foo(x: T) = "Package 1!"
        def bar = Thread.currentThread.getStackTrace()(1).getClassName()
    class B extends A[Int]
    class C extends A[String]
}

package package2 {
    inline trait A[T: Specialized]:
        def foo(x: T) = "Package 2!"
        def bar = Thread.currentThread.getStackTrace()(1).getClassName()
    class B extends A[Int]
    class C extends A[String]
}

@main def Test = 
    val b = package1.B()
    val c = package2.C()
    assert(b.foo(10) == "Package 1!")
    assert(c.foo("Hello World") == "Package 2!")
    
    val d = new package1.A[Int]() {}
    val e = new package2.A[Int]() {}
    assert(d.bar == "package1.A$impl$scala$Int")
    assert(e.bar == "package2.A$impl$scala$Int")
