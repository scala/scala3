class Test {
  def foo(): Unit = {
    val x: String|Null = ???
    if (x != null) {
      val y = x.length
    } else {
      val y = x
    }
  }
}
