object Test:

  def test() =
    val x: Any^ = "abc"
    val y: Object @scala.annotation.retains[x.type] = ???
    val z: Object @scala.annotation.retains[x.type | caps.any.type] = y: Object @annotation.retains[x.type]

