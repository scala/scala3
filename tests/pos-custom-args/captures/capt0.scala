object Test:

  def test() =
    val x: {*} Any = "abc"
    val y: Object @scala.annotation.retains(x) = ???
    val z: Object @scala.annotation.retains(x, *) = y: Object @annotation.retains(x)

