inline trait C[S]: // error: Inlining of inline traits looped, which will create an infinitely long program. This is not allowed.
   def v(x: S): S = x
   def w: Unit = 
      val x = new D[S] {}
      println("w")

inline trait D[S]: // error: Inlining of inline traits looped, which will create an infinitely long program. This is not allowed.
   def v(x: S): S = x
   def w: Unit = 
      val x = new C[S] {}
      println("w")
