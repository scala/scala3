class Foo(i: Int) {
  def this() = this({
    def bar() = {
      println(Thread.currentThread.getStackTrace.apply(1).getMethodName)
      5
    }
    bar()
  })

  def this(i: String) = this({
    def bar() = {
      println(Thread.currentThread.getStackTrace.apply(1).getMethodName)
      5
    }
    bar()
  })
}

class Bar(i: Int) {
  def this() = this({
    def bar() = {
      println(Thread.currentThread.getStackTrace.apply(1).getMethodName)
      5
    }
    bar()
  })
}

object Test {
  def main(args: Array[String]): Unit = {
    new Foo()
    new Foo("")
    new Bar()
  }
}
