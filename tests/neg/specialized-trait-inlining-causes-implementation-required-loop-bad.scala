//> using options -language:experimental.specializedTraits

// A[Char] => A$sp$Char => C$sp$Char, but only once we inline the body of C$sp$Char do we realise that we need 
// C$impl$Char as well.

inline trait C[S: Specialized]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new C[S] {} // error: Inlining of inline traits looped, which will create an infinitely long program. This is not allowed.
      println("w")

inline trait A[T: Specialized]:
   def x(y: C[T]): Unit = println("x")

def main = 
   val y = new A[Char] {}
