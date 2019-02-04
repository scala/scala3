object Test1 {
  trait T[A]

  def foo[S[_], A] given (ev: given T[A] => T[S[A]]): Unit = ()
  implicit def bar[A] given (ev: T[A]): T[List[A]] = ???

  foo[List, Int]
}

object Test2 {
  trait T
  trait S

  def foo given (ev: given T => S): Unit = ()
  implicit def bar given (ev: T): S = ???

  foo
}
