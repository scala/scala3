class Foo[A] {
  def map[B](f: A => B)(implicit bf: Int): B = ???
}

class Test[T](f: Foo[String] => T)

object o {

  implicit val x = 3  // error

  implicit def y = "abc"   // error

  implicit object a extends Test(_ map identity)  // error
  implicit object b extends Test(_ map identity) // error // error: cyclic reference
}
