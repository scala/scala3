//> using options -language:experimental.inlineTraits
inline trait C[S]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new C[S] {}
      println("w")

class T extends C[Int]  // error: Inlining of inline traits looped.
