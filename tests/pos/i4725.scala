object Test1 {
  trait T[A]

  def foo[S[_], A](using ev: T[A] ?=> T[S[A]]): Unit = ()
  implicit def bar[A](using ev: T[A]): T[List[A]] = ???

  foo[List, Int]
}

object Test2 {
  trait T
  trait S

  def foo(using ev: T ?=> S): Unit = ()
  implicit def bar(using ev: T): S = ???

  foo
}
