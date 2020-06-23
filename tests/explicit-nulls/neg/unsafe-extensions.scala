class Extensions {
  def (s: String).ext(ss: String): String = s + ss

  def f = {
    val x: String | Null = ???
    val y: String = ???

    x.ext(y) // error
    x.ext(x) // error
    y.ext(x) // error
    y.ext(y)
  }

  // i7828
  def g = {
    val x = "hello, world!".split(" ").map(_.length) // error
  }
}