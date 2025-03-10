//> using options -Werror

object Native {
  class O {
    def f: String = "F"
  }
  class M extends O
}

object Types {
  opaque type N = Native.O
  opaque type GS = Native.M

  type S = N | GS

  object S:
    def apply(): S = new N

  extension (s: S)
    def f: String = "S"
}

import Types.*

object Main {
  def main(args: Array[String]): Unit = {
    val v: S = S()
    println(v.f)
  }
}
