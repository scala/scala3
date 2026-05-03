inline trait C[S]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new D[S] {} // error: Inlining of inline traits looped, which will create an infinitely long program. This is not allowed.
      println("w")

inline trait D[S] extends C[S]
