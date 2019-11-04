trait D {
  private val x = "xxxx should appear twice"
  private object xxxx { Console.println(x) }
  def get_xxxx: AnyRef = xxxx

  private val z = "zzzz should appear twice"
  private lazy val zzzz = new ZZZZ
  class ZZZZ { Console.println(z) }
  def get_zzzz: AnyRef = zzzz
}

trait E extends D {
  def f(): Unit = {
    val y = "yyyy should appear twice"
    object yyyy {
      val x1 = get_xxxx
      val z1 = get_zzzz
      Console.println(y)
    }
    yyyy
  }
}
class C extends E {}
object Test extends D {
  object E
  def main(args : Array[String]): Unit = {
    new C().f()
    new C().f()
  }
}
