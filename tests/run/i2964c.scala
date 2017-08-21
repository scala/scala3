
object Test {
  def main(args: Array[String]): Unit = {
    new Foo().foo
    new Bar().bar
    new Bar().bar2
    new Baz().baz
  }
}

class Foo {
  def foo: Unit = {
    class Inner {
      println(this.getClass)
    }
    new Inner
  }
}
class Bar {
  def bar: Unit = {
    class Inner {
      println(this.getClass)
    }
    new Inner
  }

  def bar2: Unit = {
    class Inner {
      println(this.getClass)
    }
    new Inner
  }
}
class Baz {
  def baz: Unit = {
    class Inner {
      println(this.getClass)
    }
    new Inner
  }
}
