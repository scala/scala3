class Foo(x: Partial[String]) {
  var name: String = _
  name.size    // error

  name = "hello, world"
  name.size

  val y = name
  y.size

  name = x
}

class Bar(x: Partial[String]) {
  var name: String = x
  name.size    // error

  name = "hello, world"
  name.size

  name = x
}