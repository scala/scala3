object Main {
  class A {
    def foo(x: Int) = x
    def bar = foo(42)
  }
  
  def foo = 2
  def bar = foo
  
  @DoNotDCE
  def testEntry: Unit = {
    val x = new A
    try {
      x.foo(3)
    } catch {
      case _: dotty.runtime.DeadCodeEliminated => System.out.println("Main.A.foo")
      case _: Throwable => ()
    }
    /*Utils.checkDCE("Main.A.foo", x.foo(3))
    Utils.checkDCE("Main.A.bar", x.bar)
    Utils.checkDCE("Main.bar", bar)
    Utils.checkNotDCE("Main.foo", foo)*/
  }
  
  def main(args: Array[String]): Unit = {
    foo
  }
}