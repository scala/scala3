object Test1 {
  trait T[A]

  def foo[S[_], A] with (ev: T[A] |=> T[S[A]]): Unit = ()
  implicit def bar[A] with (ev: T[A]): T[List[A]] = ???

  foo[List, Int]
}

object Test2 {
  trait T
  trait S

  def foo with (ev: T |=> S): Unit = ()
  implicit def bar with (ev: T): S = ???

  foo
}
