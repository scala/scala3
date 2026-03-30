//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized]

@main def main =
    val x = new Foo[Int] {}
