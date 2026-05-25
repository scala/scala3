//> using options -language:experimental.specializedTraits

@main def Test = 
    val b = package1.B()
    val c = package1.C()
    assert(b.foo(10) == "Package 1!")
    assert(c.foo("Hello World") == "Package 1!")
    
    val d = new package1.Outer.Inner.A[Int]() {}
    println(d.bar)
    assert(d.bar == "package1.Outer$Inner$$A$impl$scala$Int")
