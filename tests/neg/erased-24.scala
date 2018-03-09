// Could become a run test if we had totality checking for erased arguments

object Test {

  def main(args: Array[String]): Unit = {
    println(fun(new Bar))
  }

  def fun(erased foo: Foo): foo.X = { // error
    null.asInstanceOf[foo.X] // error
  }

  def fun2(erased foo: Foo)(erased bar: foo.B): bar.X = { // error // error
    null.asInstanceOf[bar.X] // error
  }
}

class Foo {
  type X
  type B <: Bar
}

class Bar extends Foo {
  type X = String
}
