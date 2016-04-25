class X(val x: Object) extends AnyVal
class Y(val y: String) extends AnyVal
object Test {
  def main(args: Array[String]) = {
    val x = new X(null)
    val y = new Y(null)
    println(x.hashCode())
    println(y.hashCode())

    val r = Set(new Y(null))
    println(r.toString)
  }
}