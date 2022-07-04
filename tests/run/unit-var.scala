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
    assert(!f.getClass.getDeclaredFields.exists(_.getName.startsWith("foo")), "field foo not erased")
  }
}

class Foo {
  var foo: Unit = {
    println("foo")
  }

  foo = {
    println("foo2")
  }
}
