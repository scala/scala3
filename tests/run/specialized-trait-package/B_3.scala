//> using options -language:experimental.specializedTraits
class B extends package1.A[Int]
class C extends package2.A[Int]

@main def Test = 
    val b = B()
    val c = C()
    assert(b.foo(10) == "Package 1!")
    assert(c.foo(11) == "Package 2!")

    val d = new package1.A[Int]() {}
    val e = new package2.A[Int]() {}
    assert(d.bar == "package1.A$impl$scala$Int")
    assert(e.bar == "package2.A$impl$scala$Int")

    import package1.A
    import package2.A as A2
    val f = new A[Int]() {}
    val g = new A2[Int]() {}
    assert(d.bar == "package1.A$impl$scala$Int")
    assert(e.bar == "package2.A$impl$scala$Int")
