class Foo {
  import java.util.function._
  import java.util.stream._

  // Assignment context
  val p: Predicate[String]|Null = (x) => true

  // Method invocation context
  val s: Stream[String] = ???
  s.filter((x) => true)

  // Method overloading context
  trait MyFun {
    def apply(x: Int): String
  }

  def foo(m: MyFun|Null) = {}
  def foo(m: Int) = {}

  foo((x) => "hello")
}
