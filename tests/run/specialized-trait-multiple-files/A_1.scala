//> using options -language:experimental.specializedTraits

inline trait A[T: Specialized]:
    def foo(x: T):T = x

class B extends A[Int]
