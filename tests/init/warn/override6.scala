trait Foo {
  val name: String
  val message = "hello, " + name
}

class Bar extends Foo {
  val name = "Jack"          // warn
}