object p {

class Foo[T] {
  // Crashes:
  def f(): Foo[T] = (if (true) this else this).bar()

  // Works:
  // def f(): Foo[T] = new Bar(if (true) this else this).bar
}

implicit class Bar[A](val self: A) {
  def bar(): A = self
}

}
