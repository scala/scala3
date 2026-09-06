//> using options -language:experimental.inlineTraits
inline trait C[S]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new D[S] {}
      println("w")

inline trait D[S] extends C[S]

class Cl[T] extends D[T] // error: Inlining of inline traits looped, which will create an infinitely long program. This is not allowed.
