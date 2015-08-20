class Foo[A, B]
class Test {
  implicit val f: Foo[Int, String] = ???
  def t[A, B >: A](a: A)(implicit f: Foo[A, B]) = ???
  t(1) // error
}

