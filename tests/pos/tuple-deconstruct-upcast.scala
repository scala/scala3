object Test {
  def consume(a: Array[AnyRef]): Unit = {}

  def test =
    val (x, y): (AnyRef, AnyRef) = ("a", "b")
    val arr = Array(x)
    consume(arr) // depends on `arr` being inferred as Array[AnyRef]
}
