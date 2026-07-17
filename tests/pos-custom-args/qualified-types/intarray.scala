object IntArray {
  type NonNeg = {x: Int with x >= 0}

  case class IntArray(length: NonNeg) {
    def access(i: { v: Int with 0 <= v && v < this.length }): Int = ???
  }

  val a: (IntArray with a.length == 3) = IntArray(3)
  a.access(2)
}
