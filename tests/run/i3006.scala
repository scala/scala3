class Foo {
  def foo() = {
    def f() = println(Thread.currentThread.getStackTrace.apply(1).getMethodName)
    f()
  }
  def bar() = {
    def f() = println(Thread.currentThread.getStackTrace.apply(1).getMethodName)
    f()
  }
  def baz() = {
    def f() = println(Thread.currentThread.getStackTrace.apply(1).getMethodName)
    f()
  }
}

class Bar {
  def foo() = {
    def f() = println(Thread.currentThread.getStackTrace.apply(1).getMethodName)
    f()
  }
  def bar() = {
    def f() = println(Thread.currentThread.getStackTrace.apply(1).getMethodName)
    f()
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new Foo().foo()
    new Foo().bar()
    new Foo().baz()
    new Bar().foo()
    new Bar().bar()
  }
}
