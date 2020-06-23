import scala.language.unsafeNulls

class Extensions {
  def (s: String).ext(ss: String): String = s + ss

  def f = {
    val x: String | Null = ???
    val y: String = ???

    x.ext(y)
    x.ext(x)
    y.ext(x)
    y.ext(y)
  }

  // i7828
  def g = {
    val x = "hello, world!".split(" ").map(_.length)
  }
}