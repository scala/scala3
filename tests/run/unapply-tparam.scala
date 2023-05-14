class Foo[T] {
  def unapply(x: Int): Option[Int] = Some(4)
}

object Foo {
  def unapply[T](x: T): Option[Int] = Some(5)
}

object Bar {
  def unapply[T](x: T): Option[Int] = Some(5)
}

class Baz[T] {
  def unapply(x: Int): Option[Int] = Some(4)
}

object Baz


object Test extends App {
  1 match {
    case Foo(x) => assert(x == 5)
  }

  1 match {
    case Foo[Int](x) => assert(x == 5)  // Type params on object takes precedence
  }

  1 match {
    case Bar[Int](x) => assert(x == 5)
  }

  1 match {
    case Baz[Int](x) => assert(x == 4)  // Otherwise type params are for the class
  }
}