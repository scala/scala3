//> using options -language:experimental.specializedTraits

inline trait D[R: Specialized]

inline trait C[S: Specialized]:
   def w(y: D[S]): Unit = println("w")

inline trait A[T: Specialized]:
   def x(y: C[T]): Unit = println("x")

def main = 
   val b = new A[Char] {}
