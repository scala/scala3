class Foo {

  // This case is not valid but #4 in flow6 is valid, because non-lazy value definations
  // exist between forward references
  def fr(): Unit = {
    val z = implicitly[Int] // error: forward reference is not allowed here
    val x: String|Null = ???
    if (x == null) return ()
    implicit val y: Int = x.length
  }
}
