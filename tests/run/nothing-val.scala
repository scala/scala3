// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    try {
      println("0")
      val f = new Foo
      println("1")
      println(f.foo)
    } catch {
      case e: NotImplementedError => println("???")
    }

    assert(!classOf[Foo].getDeclaredFields.exists(_.getName.startsWith("foo")), "field foo not erased")
  }


}

class Foo {
  val foo: Nothing = {
    println("foo")
    ???
  }
}
