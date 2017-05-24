class X(x : Any)
class W {
  new X(new Z() with Y) {}
  trait Y { def y = () }
}
class Z(r : Any) { def this() = this(null) }

object Test {
  def main(args: Array[String]) = {
    val w = new W
  }

}
