object Test {
  class C[T]
  implicit def fGen[T](implicit ev: T): C[T] = ???
  implicit def fString(implicit ev: C[Int]): C[String] = ???
  implicit def fInt: C[Int] = ???

  implicitly[C[String]]
  implicitly[C[C[String]]]
  implicitly[C[C[C[String]]]]
  implicitly[C[C[C[C[String]]]]]
  implicitly[C[C[C[C[C[String]]]]]]
  implicitly[C[C[C[C[C[C[String]]]]]]] // error: no implicit argument found (because of divergence)
}