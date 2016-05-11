object Test {
  type Id[X] = X

  def foo(x:Id[Option[Int]]) = x match {
    case Some(n) => "foo"
    case None => "bar"
  }

  foo(Some(3)) // "foo"
  foo(None) // "bar"
}