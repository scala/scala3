//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized]:
    inline def a[S: Specialized](b: S): Boolean = a[S](b)

@main def main =
    val x = new Foo[Int]() {}
    x.a(100) // error: inline limit exceeded
