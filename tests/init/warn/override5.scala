trait Foo {
  def name: String

  val message = "hello, " + name
}

class Bar extends Foo {
  val name = "Jack"       // warn
}


trait Zen {
  val name: String

  val message = "hello, " + name
}

class Tao extends Zen {
  val name = "Jack"       // warn
}


trait Base {
  val name: String

  val message = "hello, " + name
}
