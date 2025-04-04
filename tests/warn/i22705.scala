//> using options -Werror

object Native {
  class Obj:
    def f: String = "F"
}

object Types {

  opaque type Node = Native.Obj

  type S = Node

  object S:
    def apply(): S = new Node

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
