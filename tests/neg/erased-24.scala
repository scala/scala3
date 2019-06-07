// Could become a run test if we had totality checking for erased arguments

object Test {

  def main(args: Array[String]): Unit = {
    println(fun(new Bar))
  }

  def fun erased (foo: Foo): foo.X = { // ok
    null.asInstanceOf[foo.X] // ok
  }

  def fun2 erased (foo: Foo) erased (bar: foo.B): bar.X = { // error
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
