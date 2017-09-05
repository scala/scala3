
object Test {
  def main(args: Array[String]): Unit = {
    new Foo
    new Bar
    new Baz
  }
}

class Foo {
  new Object {
    println(this.getClass) // Foo$$anon$1
  }
}
class Bar {
  new Object {
    println(this.getClass) // Bar$$anon$1
  }
  new Object {
    println(this.getClass) // Bar$$anon$2
  }
}
class Baz {
  new Object {
    println(this.getClass) // Baz$$anon$1
  }
}
