case class Foo(s: "abc")

object Test {

  val x: "abc" @deprecated = "abc"

  val y: "abc" = x

}
