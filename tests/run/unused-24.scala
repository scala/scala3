object Test {

  def main(args: Array[String]): Unit = {
    println(fun(new Bar))
  }

  def fun(unused foo: Foo): foo.X = {
    null.asInstanceOf[foo.X]
  }

  def fun2(unused foo: Foo)(unused bar: foo.B): bar.X = {
    null.asInstanceOf[bar.X]
  }
}

class Foo {
  type X
  type B <: Bar
}

class Bar extends Foo {
  type X = String
}
