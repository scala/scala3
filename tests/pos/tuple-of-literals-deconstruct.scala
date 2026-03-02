object Test {
  def test =
    val (x: "x", y: "y") = (("x", "y"): ("x", "y"))
    println(x)
}
