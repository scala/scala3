//> using options -language:experimental.specializedTraits
var hops = 0

inline trait A[T: Specialized]:
    def foo() = 
        hops += 1
        "A"

inline trait B[T: Specialized] extends A[T]:
    override def foo() = 
        hops += 1
        super.foo()

inline trait C[T: Specialized] extends B[T]:
    override def foo() = 
        hops += 1
        "B"

inline trait D[T: Specialized] extends C[T]:
    override def foo() = 
        hops += 1
        super.foo()

inline trait E[T: Specialized] extends D[T]:
    override def foo() = 
        hops += 1
        super.foo()

inline trait F[T: Specialized] extends E[T]:
    override def foo() = 
        hops += 1
        super.foo()

class C1 extends F[Int]

@main def Test =
    hops = 0
    val cl = C1()
    assert(cl.foo() == "B")
    assert(hops == 4)

    hops = 0
    val cl2 = new F[String]() {}
    assert(cl.foo() == "B")
    assert(hops == 4)

