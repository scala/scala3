object Test {

  def main(args: Array[String]): Unit = {
    println(fun(new Bar))
  }

  def fun(ghost foo: Foo): foo.X = {
    null.asInstanceOf[foo.X]
  }

  def fun2(ghost foo: Foo)(ghost bar: foo.B): bar.X = {
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
