object Foo {
  def foo(): Int = {
    val f: Int ?=> Int = (using x: Int) => 2 * x
    f(using 2)
  }

  val f = implicit (x: Int) => x

  ((using x: Int) => x): (Int ?=> Int) // error: no implicit argument found
}

