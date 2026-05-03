//> using options -language:experimental.specializedTraits

// Contrast with tests/neg/specialized-trait-inlining-causes-implementation-required-loop-bad.scala.
// This one is not allowed because it will loop forever when specializing. 

// A[Char] => A$sp$Char => C$sp$Char, but only once we inline the body of C$sp$Char do we realise that we need 
// C$impl$Char as well.

inline trait C[S: Specialized]:
   def v(x: S): S = x
   def w: Unit = 
      class D extends C[S] // ok: This one is actually also fine because we can make D extend C$sp$Char once we've inlined it into C$impl$Int
      println("w")

inline trait A[T: Specialized]:
   def x(y: C[T]): Unit = println("x")

def main = 
   val y = new A[Char] {}
