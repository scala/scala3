// scalajs: --skip

object Test extends App {

  val x = "abc"

  val m = x.getClass.getMethod("toString")

  assert(m.invoke(x, (Nil: List[AnyRef])*) == "abc")

  Test2.main(Array())
}


object Test2 {
  def main(args: Array[String]): Unit = {
    val x = "abc"
    val m = x.getClass.getMethod("toString")
    m.invoke(x, Nil*)
    m.invoke(x, Seq()*)
  }
}
