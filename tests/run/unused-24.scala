object Test {

  def main(args: Array[String]): Unit = {
    println(fun(new Bar))
  }

  def fun(unused foo: Foo): foo.X = {
    null.asInstanceOf[foo.X]
  }
}

class Foo {
  type X
}

class Bar extends Foo {
  type X = String
}
