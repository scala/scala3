// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    println(1)
    f.foo
    f.foo
    assert(!f.getClass.getDeclaredFields.exists(_.getName.startsWith("foo")), "field foo not erased")
  }
}

class Foo {
  val foo: Unit = {
    println("foo")
  }
}
