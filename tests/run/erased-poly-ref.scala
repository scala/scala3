//> using options -language:experimental.erasedDefinitions

object Test {

  def main(args: Array[String]): Unit = {
    fun(foo(bar(5))(bar(6)))
  }

  def fun(erased a: Int): Unit = println("fun")

  inline def foo[P](erased x: Int)(erased y: Int): Int = 0

  inline def bar(x: Int) =  {
    x
  }
}
