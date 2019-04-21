
import Location._

object Test {
  val loc1 = location
  def main(args: Array[String]): Unit = {
    foo(loc1)
    foo(location)
    foo

    def bar = {
      foo
      val baz = foo
    }
    bar

    val f = (i: Int) => foo
    f(0)
  }

  def foo(implicit location: Location): Unit = {
    println("foo " + location)
  }
}
