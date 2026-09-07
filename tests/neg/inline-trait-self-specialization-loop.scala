//> using options -language:experimental.specializedTraits

inline trait C[S]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new C[Int] {}
      println("w")

def main = 
   val y = new C[Int] {} // error: Inlining of inline traits loops which would create an infinitely long program. This is not allowed.
