trait Foo[In] { type Out }

object Test {
  def fooInt: Foo[Int] { type Out = String } = ???
  implicit def str: String = ???

  def test1[A](f1: Foo[A])(implicit f2: f1.Out) = ???
  def test2[A](implicit f1: Foo[A], f2: f1.Out) = ???

  test1(fooInt)   // OK
  test2           // error
}

object Test2 {
  implicit def fooInt: Foo[Int] { type Out = String } = ???
  implicit def fooString: Foo[String] { type Out = Boolean } = ???
  implicit def fooBoolean: Foo[Boolean] { type Out = Double } = ???

  def test3[A](f1: Foo[A], f2: Foo[f1.Out])(implicit f3: Foo[f2.Out]): f3.Out = ???
  def test4[A](implicit f1: Foo[A], f2: Foo[f1.Out], f3: Foo[f2.Out]): f3.Out = ???

  val t3 = test3(fooInt, fooString)
  t3: Double
  val t4 = test4  // error
  t4: Double
}
