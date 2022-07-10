// scalajs: --skip

class Foo extends bla.Base {
  class Inner {
    println(foo())
    println(bar)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    new f.Inner
  }
}
