trait Foo {
  def name: String

  val message = "hello, " + name
}

class Bar extends Foo {
  val name = "Jack"       // error
}


trait Zen {
  val name: String

  val message = "hello, " + name
}

class Tao extends Zen {
  val name = "Jack"       // error
}


trait Base {
  val name: String

  val message = "hello, " + name
}
