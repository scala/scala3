class Foo {
  def test[A]: (List[Int] | A) => Int = {
    case ls: List[Int] => ls.head       // error, A = List[String]
    case _ => 0
  }

  def test2: List[Int] | List[String] => Int = {
    case ls: List[Int] => ls.head       // error
    case _ => 0
  }

  trait A[T]
  trait B[T]

  // suppose: class C extends A[Int] with B[String]
  def test3[X]: A[X] | B[X] => Int = {
    case ls: A[X] => 4                 // error
    case _ => 0
  }

  def test4[A](x: List[Int] | (A => Int)) = x match {
    case ls: List[Int] => ls.head       // ok, List decomposes to Some and None
    case _ => 0
  }
}
