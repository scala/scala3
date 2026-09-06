//> using options -language:experimental.specializedTraits

inline trait Numeric2[T: Specialized]

inline trait A[T: {Numeric2, Specialized}]:
    def bar(x: T): T = x

given Numeric2[Int] = new Numeric2[Int] {}

class B extends A[Int]:
    def baz(x: Int): Int = x
