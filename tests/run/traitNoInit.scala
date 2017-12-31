trait NoInit {
  def meth(x: Int): Int
}

trait WithInit {
  val i = 1
  def meth(x: Int): Int
}

trait Bar(x: Int)

class NoInitClass extends NoInit() with Bar(1) {
  def meth(x: Int) = x
}

class WithInitClass extends WithInit() with Bar(1) {
  def meth(x: Int) = x
}

object Test {
  def hasInit(cls: Class[_]) = cls.getMethods.map(_.toString).exists(_.contains("$init$"))
  def main(args: Array[String]): Unit = {
    val noInit = new NoInitClass {}
    val withInit = new WithInitClass {}

    assert(!hasInit(noInit.getClass))
    assert(hasInit(withInit.getClass))
  }
}
