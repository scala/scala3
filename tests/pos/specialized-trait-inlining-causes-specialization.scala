//> using options -language:experimental.specializedTraits

inline trait D[R: Specialized]

inline trait C[S: Specialized]:
   def w(y: D[S]): Unit = println("w")

inline trait A[T: Specialized]:
   def x(y: C[T]): Unit = println("x")

class B extends A[Char]
