//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits

inline trait List[+T: Specialized]
case object Nil extends List[Nothing]

def foo(x: List[Int]) = println("Foo")

def main =
    foo(Nil) // error: This use of variance is incompatible with specialized traits.
