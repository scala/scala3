import scala.quoted.*
class Foo {
  def foo(using Quotes): Unit = {
    val expr ='{
      val a = 3
      println("foo")
      2 + a
    }
  }
}
