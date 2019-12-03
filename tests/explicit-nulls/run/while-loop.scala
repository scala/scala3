
object Test {
  class C(val x: Int, val next: C|Null)

  def main(args: Array[String]): Unit = {
    var xs: C|Null = C(1, C(2, C(3, null)))
    while (xs != null) {
      println(xs.x)
      xs = xs.next
    }
  }
}