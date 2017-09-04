object Test {
  def main(args: Array[String]): Unit = {
    new B
  }
}

class A(x: Int)

class B extends A({ class Foo extends EmptyClass; 1 })
