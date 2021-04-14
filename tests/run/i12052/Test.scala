import MirrorType._
object Test {
  def main(args: Array[String]): Unit = {
    val ctx = new MyContext();
    import ctx._
    val tup = ("foo", 1)
    assert(tup.mirrorType.isInstanceOf[String])
  }
}