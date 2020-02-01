import scala.quoted._
class Foo {
  def foo(using QuoteContext): Unit = {
    val expr ='{
      val a = 3
      println("foo")
      2 + a
    }
  }
}
