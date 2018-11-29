
object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    println(f.foo)
    println(f.foo)
    assert(!f.getClass.getDeclaredFields.exists(_.nn.getName.startsWith("foo")), "field foo not erased")
  }
}

class Foo {
  val foo: Null = {
    println("foo")
    null
  }
}
