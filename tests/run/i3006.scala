// scalajs: --skip

class Foo {
  def foo() = {
    def f() = Thread.currentThread.getStackTrace.apply(1).getMethodName
    f()
  }
  def bar() = {
    def f() = Thread.currentThread.getStackTrace.apply(1).getMethodName
    f()
  }
  def baz() = {
    def f() = Thread.currentThread.getStackTrace.apply(1).getMethodName
    f()
  }
}

class Bar {
  def foo() = {
    def f() = Thread.currentThread.getStackTrace.apply(1).getMethodName
    f()
  }
  def bar() = {
    def f() = Thread.currentThread.getStackTrace.apply(1).getMethodName
    f()
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    List(new Foo().foo(), new Foo().bar(), new Foo().baz()).sorted.foreach(println)
    List(new Bar().foo(), new Bar().bar()).sorted.foreach(println)
  }
}
