trait F[A, -E]
object Test {
  def empty[A](value: A): F[A, Any] = ???

  def hof[R](f: (p: AnyRef) => F[R, p.type]): F[R, Any] = ???

  hof { p =>
    empty(42)
  }
}