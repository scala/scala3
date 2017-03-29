object Test {

  def bar(tl: => String) = {
    val x = tl _ // error
    val y = x _ // error
    val s: String = x() // error
  }

}
