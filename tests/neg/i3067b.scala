class Foo[A] {
  def map[B](f: A => B)(implicit bf: Int): B = ???
}

class Test[T](f: Foo[String] => T)

object o {
  implicit object b extends Test(_ map identity)  // error: type needs to be given // error: cyclic reference
}
