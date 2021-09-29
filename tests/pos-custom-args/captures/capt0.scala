object Test:

  def test() =
    val x: {*} Any = "abc"
    val y: Object @scala.retains(x) = ???
    val z: Object @scala.retains(x, *) = y: Object @scala.retains(x)

