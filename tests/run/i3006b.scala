class Foo(val x: String) {

  def this() = this({
    def bar() = Thread.currentThread.getStackTrace.apply(1).getMethodName.nn
    bar()
  })

  def this(i: Int) = this({
    def bar() = Thread.currentThread.getStackTrace.apply(1).getMethodName.nn
    bar()
  })
}

class Bar(val x: String) {
  def this() = this({
    def bar() = Thread.currentThread.getStackTrace.apply(1).getMethodName.nn
    bar()
  })
}

object Test {
  def main(args: Array[String]): Unit = {
    List(new Foo().x, new Foo(2).x).sorted.foreach(println)
    println(new Bar().x)
  }
}
