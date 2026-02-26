//> using options -language:experimental.specializedTraits

// A[Char] => A$sp$Char, A$impl$Char => C$sp$Char, but only once we inline the body of C$sp$Char do we realise that we need 
// B$impl$Char as well. 

// TODO: However, arguably we don't actually need B$impl$Char

inline trait B[E: Specialized]

inline trait C[S: Specialized]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new B[S] {}
      println("w")

inline trait A[T: Specialized]:
   def x(y: C[T]): Unit = println("x")

def foo(x: B[Char]) = "A"

def main = 
   val y = new A[Char] {}

/*
inline trait B[E: Specialized]

inline trait C[S: Specialized]:
   def v(x: S): S
   def w: Unit

inline trait A[T: Specialized]:
   def x(y: C[T]): Unit

inline trait A$sp$Char:
   def x(y: C$sp$Char): Unit

class A$impl$Char:
   def x(y: C$sp$Char): Unit = println("x")

inline trait B$sp$Char

class B$impl$Char

inline trait C$sp$Char:
   def v(x: Char): Char
   def w: Unit

def foo(x: B$sp$Char) = "A"

def main = 
   val y = new A$impl$Char() {}

*/
