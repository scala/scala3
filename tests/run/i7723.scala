object Test with
  @scala.annotation.alpha("A")
  class B(val i: Int = 1)

  def main(args: Array[String]): Unit =
    assert(B().i == 1)

@scala.annotation.alpha("AA")
  class BB(val i: Int = 1)
