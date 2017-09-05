
object Test {
  def main(args: Array[String]): Unit = {
    new foo.bar.Foo
    new foo.Foo
    new Foo
  }
}

package foo {
  package bar {
    class Foo {
      new Object {
        println(this.getClass) // Foo$$anon$1
      }
      new Object {
        println(this.getClass) // Foo$$anon$2
      }
    }
  }
  class Foo {
    new Object {
      println(this.getClass) // Foo$$anon$1
    }
  }
}

class Foo {
  new Object {
    println(this.getClass) // Foo$$anon$1
  }
}
