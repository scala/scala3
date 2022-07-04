// scalajs: --skip

object Test {

  def main(args: Array[String]): Unit = {
    List(foo(1), foo("a"), baz(2), baz("b")).sorted.foreach(println)
  }

  def foo[X <: Int](x: X) = {
    def bar = thisMethodName()
    bar
  }

  def foo(x: String) = {
    def bar = thisMethodName()
    bar
  }

  def baz[X <: Int](x: X) = {
    object qux {
      override def toString() = thisMethodsClassName()
    }
    qux.toString()
  }

  def baz(x: String) = {
    object qux {
      override def toString() = thisMethodsClassName()
    }
    qux.toString()
  }

  def thisMethodName() = Thread.currentThread().getStackTrace()(2).getMethodName

  def thisMethodsClassName() = Thread.currentThread().getStackTrace()(2).getClassName
}
