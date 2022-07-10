// scalajs: --skip

trait NoInit {
  def meth(x: Int): Int
}

trait WithInit {
  val i = 1
  def meth(x: Int): Int
}

trait Bar(x: Int)

object Test {
  def hasInit(cls: Class[_]) = cls.getMethods.map(_.toString).exists(_.contains("$init$"))
  def main(args: Array[String]): Unit = {
    assert(!hasInit(classOf[NoInit]))
    assert(hasInit(classOf[WithInit]))
    assert(!hasInit(classOf[Bar]))
  }
}
