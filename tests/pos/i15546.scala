trait Foo[F[_]]

object Bug {
  def apply[F[_]: Foo](
    await: Boolean,
    whatever: Int = 0
  ): Nothing = ???

  def apply[F[_]: Foo]: Nothing =
    apply[F](false)
}