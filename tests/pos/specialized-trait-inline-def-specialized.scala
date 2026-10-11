//> using options -language:experimental.specializedTraits

inline trait Vec[T: Specialized](val x: T)

inline def foo[T: Specialized](v: Vec[T]) = v.x

@main def Test = 
    val v = new Vec[Int](10) {}
    println(foo(v))
