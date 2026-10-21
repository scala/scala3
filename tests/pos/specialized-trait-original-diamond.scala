//> using options -language:experimental.specializedTraits

inline trait A[T: Specialized]:
    def foo = 10

inline trait B extends A[Int]
inline trait C extends A[Int]

class Bar extends B with C
