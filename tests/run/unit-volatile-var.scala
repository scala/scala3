// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
    f.foo
    f.foo = {
      println("foo3")
    }
    f.foo
    assert(f.getClass.getDeclaredFields.exists(_.getName.startsWith("foo")), "@volatile field foo erased")
  }
}

class Foo {
  @volatile var foo: Unit = {
    println("foo")
  }

  foo = {
    println("foo2")
  }
}
