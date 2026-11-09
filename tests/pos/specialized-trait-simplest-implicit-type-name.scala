//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized](val x: T)
inline trait Bar[T: Specialized]

@main def main =
    val x = new Foo(10) {} // Type name not provided explicitly; this should still be specialized to Int.
    val y = new Bar() {}   // This will be specialized to Nothing.
