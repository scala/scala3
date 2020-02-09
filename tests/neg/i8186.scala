trait Test1() {
  def this(x: Int) = // error
    this()
}

trait Test2(x: Int) {
  def this() = // error
    this(3)
}
