import java.nio.file.*

class Test {
  def test(xs: Array[String]) = {
    val p1 = Paths.get("Hello")
    val p2 = Paths.get("Hello", "World")
    val p3 = Paths.get("Hello", "World", "!")
    val p4 = Paths.get("Hello", xs*)
  }
}
