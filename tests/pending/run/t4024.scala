object Test extends dotty.runtime.LegacyApp {

  val x = "abc"

  val m = x.getClass.getMethod("toString")

  assert(m.invoke(x, (Nil: List[AnyRef]): _*) == "abc")

  Test2.main(Array())
}


object Test2 {
  def main(args: Array[String]): Unit = {
    val x = "abc"
    val m = x.getClass.getMethod("toString")
    m.invoke(x, Nil: _*)
    m.invoke(x, Seq(): _*)
  }
}
