class Foo {
  def test[A]: (List[Int] | A) => Int = {
    case ls: List[Int] => ls.head       // warn: unchecked
    case _ => 0
  }

  def test2: List[Int] | List[String] => Int = {
    case ls: List[Int] => ls.head       // warn: unchecked
    case _ => 0
  }

  trait A[T]
  trait B[T]

  // suppose: class C extends A[Int] with B[String]
  def test3[X]: A[X] | B[X] => Int = {
    case ls: A[X] => 4                 // warn
    case _ => 0
  }

  def test4[A](x: List[Int] | (A => Int)) = x match {
    case ls: List[Int] => ls.head       // warn, List extends Int => T
    case _ => 0
  }

  final class C[T] extends A[T]

  def test5[T](x: A[T] | B[T] | Option[T]): Boolean = x.isInstanceOf[C[String]] // warn

  def test6[T](x: A[T] | B[T] | Option[T]): Boolean = x.isInstanceOf[C[T]]

  def test7[A](x: Option[Int] | (A => Int)) = x match {
    case ls: Option[Int] => ls.head       // OK, Option decomposes to Some and None
    case _ => 0
  }

  def test8(x: List[Int] | A[String]) = x match {
    case ls: List[Int] => ls.head        // OK, List decomposes to :: and Nil
    case _ => 0
  }
}
