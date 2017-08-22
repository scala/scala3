object Test {
  def main(args: Array[String]): Unit = {
    new Foo
    new bar.Bar
  }
}

class Foo {
  new Object { println(this.getClass.getName) }
}

package bar {
  package baz { }
  class Bar {
    new Object { println(this.getClass.getName) }
  }
  package quxx { }
}
