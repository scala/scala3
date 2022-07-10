// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit =
    val obj = new JavaClass_2
    assert(obj.a == 1)
    assert(obj.b == 2)
    assert(obj.c() == 3)
}
