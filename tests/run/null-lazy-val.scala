// scalajs: --skip

object Test {

  def main(args: Array[String]): Unit = {
    val f = new Foo
    println(1)
    println(f.foo)
    println(2)
    println(f.foo)

    // TODO: Erase
    // Currently not erasing fields for lazy vals
    assert(f.getClass.getDeclaredFields.exists(_.getName.startsWith("foo")), "Field foo erased. Optimized accidentally?")
  }


}

class Foo {
  lazy val foo: Null = {
    println("foo")
    null
  }
}
