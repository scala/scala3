import scala.quoted._
class Foo {
  def foo given QuoteContext: Unit = {
    val expr ='{
      val a = 3
      println("foo")
      2 + a
    }
  }
}
