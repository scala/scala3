object Test {
  implicit val theAnswer: Int = 42

  type Swap[A, B] = (B, A)

  def foo[M[_, _], T, S](x: M[T, S])(implicit ev: T) = ev

  val a: Swap[Int, String] = ("hi", 1)

  foo(a)
}
