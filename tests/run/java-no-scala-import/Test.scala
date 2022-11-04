// scalajs: --skip

object Test extends App {
  val c = new Config()
  c.setLongObj(10)
  println(c.getLongObj)
  val l = c.longLength("test")
}
