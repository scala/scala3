import Location._

object Test {
  val loc1 = location
  def main(args: Array[String]): Unit = {
    foo(loc1)
  }

  def foo(implicit location: Location): Unit = {
    println("foo " + location)
  }
}
