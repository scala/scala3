object Test {

  def main(args: Array[String]): Unit = {
    foo(1)
    foo("a")
    baz(2)
    baz("b")
  }

  def foo[X <: Int](x: X) = {
    def bar = printlnThisMethodName()
    printlnThisMethodName()
    bar
  }

  def foo(x: String) = {
    def bar = printlnThisMethodName()
    printlnThisMethodName()
    bar
  }

  def baz[X <: Int](x: X) = {
    object qux {
      override def toString() = {
        printlnThisMethodsClassName()
        "a"
      }
    }
    printlnThisMethodName()
    qux.toString()
  }

  def baz(x: String) = {
    object qux {
      override def toString() = {
        printlnThisMethodsClassName()
        "b"
      }
    }
    printlnThisMethodName()
    qux.toString()
  }

  def printlnThisMethodName() =
    println(Thread.currentThread().getStackTrace()(2).getMethodName)

  def printlnThisMethodsClassName() =
    println(Thread.currentThread().getStackTrace()(2).getClassName)
}
