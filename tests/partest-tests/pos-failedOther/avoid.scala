abstract class C {
  def y: Any
}

object test {
  val x = new C{
    def y: String = "abc"
  }
  val z: String = x.y
}
