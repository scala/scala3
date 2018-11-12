object Foo {
  def foo(): Int = {
    val f: Int |=> Int = (x: Int) |=> 2 * x
    f with 2
  }

  val f = implicit (x: Int) => x

  ((x: Int) |=> x): (Int |=> Int) // error: no implicit argument found
}
