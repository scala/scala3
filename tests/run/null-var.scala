// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    println(f.foo)
    f.foo
    f.foo = {
      println("foo3")
      null
    }
    println(f.foo)
    assert(!f.getClass.getDeclaredFields.exists(_.getName.startsWith("foo")), "field foo not erased")
  }
}

class Foo {
  var foo: Null = {
    println("foo")
    null
  }

  foo = {
    println("foo2")
    null
  }
}
