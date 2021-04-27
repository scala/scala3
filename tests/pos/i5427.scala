trait Foo[In] { type Out }

object Test {
  implicit def fooInt: Foo[Int] { type Out = String } = ???
  implicit def str: String = ???

  def test1[A](f1: Foo[A])(implicit f2: f1.Out) = ???
  def test2[A](implicit f1: Foo[A], f2: f1.Out) = ???

  test1(fooInt) // OK
  test2         // OK
}

object Test2 {
  implicit def fooInt: Foo[Int] { type Out = String } = ???
  implicit def fooString: Foo[String] { type Out = Boolean } = ???
  implicit def fooBoolean: Foo[Boolean] { type Out = Double } = ???

  def test3[A](f1: Foo[A], f2: Foo[f1.Out])(implicit f3: Foo[f2.Out]): f3.Out = ???
  def test4[A](implicit f1: Foo[A], f2: Foo[f1.Out], f3: Foo[f2.Out]): f3.Out = ???

  val t3 = test3(fooInt, fooString)
  t3: Double
  val t4 = test4[Int]
  t4: Double
}

object Test3 {
  def fooInt: Foo[Int] { type Out = String } = ???
  implicit def istr: String = ???
  implicit def iint: Int = ???

  def test5[A](implicit f1: Foo[A] = fooInt, f2: f1.Out) = f2

  val t5 = test5
    // used to succeed with just one local implicit `istr`
    // but failed if a competing implicit `iint` was added.
  t5: String
}

object Test4 {
  implicit def fooInt: Foo[Int] { type Out = String } = ???
  def str: String = ???

  def test6[A](implicit f1: Foo[A], f2: f1.Out = str) = f2

  val t6 = test6
  t6: String
}
