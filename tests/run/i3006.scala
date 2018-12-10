class Foo {
  def foo() = {
    def f() = Thread.currentThread.getStackTrace.apply(1).getMethodName.nn
    f()
  }
  def bar() = {
    def f() = Thread.currentThread.getStackTrace.apply(1).getMethodName.nn
    f()
  }
  def baz() = {
    def f() = Thread.currentThread.getStackTrace.apply(1).getMethodName.nn
    f()
  }
}

class Bar {
  def foo() = {
    def f() = Thread.currentThread.getStackTrace.apply(1).getMethodName.nn
    f()
  }
  def bar() = {
    def f() = Thread.currentThread.getStackTrace.apply(1).getMethodName.nn
    f()
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    List(new Foo().foo(), new Foo().bar(), new Foo().baz()).sorted.foreach(println)
    List(new Bar().foo(), new Bar().bar()).sorted.foreach(println)
  }
}
