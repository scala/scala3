object Test1 {
  trait T[A]

  def foo[S[_], A](implicit ev: implicit T[A] => T[S[A]]): Unit = ()
  implicit def bar[A : T]: T[List[A]] = ???

  foo[List, Int]
}

object Test2 {
  trait T
  trait S

  def foo(implicit ev: implicit T => S): Unit = ()
  implicit def bar(implicit ev: T): S = ???

  foo
}
