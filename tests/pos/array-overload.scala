class Test {
  object A {
    def apply(x: Byte, xs: Byte*): Array[Byte] = ???
    def apply(x: Int, xs: Int*): Array[Int] = ???
  }

  val x: Array[Byte] = A.apply(1, 2)
}
