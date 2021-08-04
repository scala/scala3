object test1:
  class Foo(
    val x: String,
    val y: Option[x.type]
  )

  class Bar extends Foo("bar", Some("bar"))

object test2:
  trait Foo(
    val x: String,
    val y: Option[x.type]
  )

  class Bar extends Foo("bar", Some("bar"))
