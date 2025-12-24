// https://github.com/scala/scala3/issues/12679

object Example:
  def foo[F[_]](qux: String, quux: String = ""): F[Unit] = ???

  def foo[F[_]](qux: Boolean): F[Unit] = ???

  def example[F[_]](maybeQux: Option[String], bool: Boolean) =
    maybeQux.fold(foo[F](bool))(foo[F](_))

object Example2:
  def foo(qux: String, quux: String = ""): Unit = ???

  def foo(qux: Boolean): Unit = ???

  def example(maybeQux: Option[String], bool: Boolean) =
    maybeQux.fold(foo(bool))(s => foo(s))