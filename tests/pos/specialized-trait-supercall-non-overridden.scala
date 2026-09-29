//> using options -language:experimental.specializedTraits
inline trait A[T: Specialized]:
    def foo = 10

inline trait B extends A[Int]:
    def bar = super.foo
