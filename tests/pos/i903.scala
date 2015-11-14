object Test {
  def contains(s: String, i: Int) = true
  def test1 = {
    val f = contains("", (_: Int))
    val ff = contains("", ((_: Int)))
    val g: Int => Boolean = contains("", (_))
    val gg: Int => Boolean = contains("", ((_)))
    f.apply(0)
    //     sandbox/eta.scala:4: error: type mismatch:
    //  found   : Int => Int
    //  required: Int
    //     val f = contains("", (_: Int))
    //                          ^
    // sandbox/eta.scala:5: error: apply is not a member of Boolean(f)
    //     f.apply(0)
    //      ^
  }

  def test2 = {
    val f = "".contains("", (_: Int)) // dotc:
    f.apply(0)
    // sandbox/eta.scala:18: error: apply is not a member of Boolean(f)
    //     f.apply(0)
    //       ^
  }
}
