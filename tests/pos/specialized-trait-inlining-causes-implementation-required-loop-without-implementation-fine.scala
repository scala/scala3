//> using options -language:experimental.specializedTraits

// This one is fine because we only ask for an $sp$ trait interface in C and not an $impl$ class

inline trait C[S: Specialized]:
   def v(x: S): S = x
   def w: Unit = 
      class D extends C[S] 
      println("w")

inline trait A[T: Specialized]:
   def x(y: C[T]): Unit = println("x")

def main = 
   val y = new A[Char] {}
