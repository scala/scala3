class A(x: Int, y: Int) {
  def this() = {
    this()   // error
  }

  def this(x: Long) = {
    this(x.toInt)  // error
  }

  def this(a: Int) = {
    this(a, a)  // OK
    val b: Int = c
    val c = 2
  }
}