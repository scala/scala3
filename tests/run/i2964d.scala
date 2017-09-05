
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
    object Inner {
      println(this.getClass)
    }
    Inner
  }
}
class Bar {
  def bar: Unit = {
    object Inner {
      println(this.getClass)
    }
    Inner
  }

  def bar2: Unit = {
    object Inner {
      println(this.getClass)
    }
    Inner
  }
}
class Baz {
  def baz: Unit = {
    object Inner {
      println(this.getClass)
    }
    Inner
  }
}
