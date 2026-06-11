//> using options -language:experimental.specializedTraits

class C extends A[Int]

@main def Test =
    val b = B()
    val c = C()

    println(b.foo(10))
    println(c.foo(10))