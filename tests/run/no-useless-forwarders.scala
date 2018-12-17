trait A {
  def foo(a: Int): Int = a
  def bar(a: Int): Int
}

trait B {
  def bar(a: Int): Int = a
}

object Test extends A with B{
  def main(args: Array[String]) = {
    assert(!this.getClass.getDeclaredMethods.exists{x: java.lang.reflect.Method|Null => x.nn.getName == "foo"}, 
      "no forwarder is needed here")
    assert(!this.getClass.getDeclaredMethods.exists{x: java.lang.reflect.Method|Null => x.nn.getName == "bar"}, 
      "no forwarder is needed here")
  }
}
