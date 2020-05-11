import scala.quoted._
object Foo {
  def f(using s: Scope) = {
    val t: s.Type[Int] = ???
    val '[ *:[$t] ] = ??? // error
  }
}