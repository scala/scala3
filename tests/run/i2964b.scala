
object Test {
  def main(args: Array[String]): Unit = {
    Foo
    Bar
    Baz
  }
}

object Foo {
  new Object {
    println(this.getClass) // Foo$$anon$1
  }
}
object Bar {
  new Object {
    println(this.getClass) // Bar$$anon$1
  }
  new Object {
    println(this.getClass) // Bar$$anon$2
  }
}
object Baz {
  new Object {
    println(this.getClass) // Baz$$anon$1
  }
}
