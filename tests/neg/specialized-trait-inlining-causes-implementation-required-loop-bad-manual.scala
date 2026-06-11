//> using options -language:experimental.specializedTraits

// Contrast with specialized-trait-inlining-causes-implementation-required-loop-bad.scala.
// and specialized-trait-inlining-causes-implementation-required-loop-without-implementation-fine.scala
// This one is not allowed because it will loop forever when specializing. The $impl$ class will
// get a loop when the body of w is inlined.

// A[Char] => A$sp$Char, but only once we inline the body of A$sp$Char do we realise that we need 
// C$impl$Char as well.

inline trait C[S: Specialized]:
   def v(x: S): S = x
   def w: Unit = 
      class D extends C[S] 
      println("w")

inline trait A[T: Specialized]:
   def x(y: C[T]): Unit = 
      val x = new C[T]() {} // error: Inlining of inline traits looped
      println("x")

def main = 
   val y = new A[Char] {}
