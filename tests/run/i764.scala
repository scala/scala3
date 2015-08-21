abstract class A {
 def foo: Int
}

trait B {
 def foo = 2
}

object Test extends A with B {
  
  def main(args: Array[String]): Unit = {
    this.foo
  }
}
