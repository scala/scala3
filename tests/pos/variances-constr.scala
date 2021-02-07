import language.`3.0-migration`

class C[+A] {

  private[this] var y: A = compiletime.uninitialized
  def getY: A = y

  class Inner(x: A) {
    y = x
  }
}

object Test {

  def main(args: Array[String]) = {
    val x = new C[String]
    val y: C[Any] = x
    val i = new y.Inner(1)
    val s: String = x.getY
    println(s)
  }
}

class CC[+A] {
  class Inner {
    def this(a: A) = this()
  }
}

