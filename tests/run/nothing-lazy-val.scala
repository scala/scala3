// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    try {
      println("0")
      val f = new Foo
      println("1")
      println(f.foo)
      println("2")
      println(f.foo)
    } catch {
      case e: NotImplementedError => println("???")
    }

    // TODO: Erase
    // Currently not erasing fields for lazy vals
    assert(classOf[Foo].getDeclaredFields.exists(_.getName.startsWith("foo")), "Field foo erased. Optimized accidentally?")
  }


}

class Foo {
  lazy val foo: Nothing = {
    println("foo")
    ???
  }
}
