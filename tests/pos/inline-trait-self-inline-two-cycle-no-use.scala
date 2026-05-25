// We throw the error only when someone tries to use the traits, because
// the theoretical infinite inlining is only an intermediate state before pruning anyway.
// At the end these traits can be pruned.

inline trait C[S]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new D[S] {}
      println("w")

inline trait D[S]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new C[S] {}
      println("w")
