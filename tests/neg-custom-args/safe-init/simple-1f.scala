class Foo(x: Partial[String]) {
  var name: String = _     // error
  name.size    // error

  name = "hello, world"
  name.size

  val y = name
  y.size

  name = x
}

class Bar(x: Partial[String]) {
  var name: String = x              // error
  name.size    // error

  name = "hello, world"
  name.size

  name = x
}