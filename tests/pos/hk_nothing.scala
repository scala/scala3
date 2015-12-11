object Test {
  def foo[M[_]]: M[Int] = ???
  foo[Nothing]
  foo
}
