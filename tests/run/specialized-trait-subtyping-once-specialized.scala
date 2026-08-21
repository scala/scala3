//> using options -language:experimental.specializedTraits
inline trait T1[T: Specialized]
inline trait T2[T: Specialized] extends T1[T]

def foo(x: T1[Int]) = println("foo")

@main def Test = 
    val x = new T2[Int]() {}
    foo(x)
