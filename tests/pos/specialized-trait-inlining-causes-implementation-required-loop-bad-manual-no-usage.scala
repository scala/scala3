//> using options -language:experimental.specializedTraits

// No usage of the spec traits so this is fine.

inline trait C[S: Specialized]:
   def v(x: S): S = x
   def w: Unit = 
      class D extends C[S]
      println("w")

inline trait A[T: Specialized]:
   def x(y: C[T]): Unit = println("x")
