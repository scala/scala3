//> using options -language:experimental.inlineTraits
inline trait C[S]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new D[S] {}
      println("w")

inline trait D[S]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new E[S] {}
      println("w")

inline trait E[S]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new C[S] {}
      println("w")

def main = 
   val x = new C[Int] {} // error: Inlining of inline traits looped.
