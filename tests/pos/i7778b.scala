class Foo[A](run: A ?=> Int) {
  def foo[T](f: T ?=> Int = run) = ()
}

