//> using options -language:experimental.specializedTraits
package owner {
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
}

@main def Test = 
    val b = owner.package1.B()
    val c = owner.package2.C()
    assert(b.foo(10) == "Package 1!")
    assert(c.foo("Hello World") == "Package 2!")
    
    val d = new owner.package1.A[Int]() {}
    val e = new owner.package2.A[Int]() {}
    assert(d.bar == "owner.package1.A$impl$scala$Int")
    assert(e.bar == "owner.package2.A$impl$scala$Int")

