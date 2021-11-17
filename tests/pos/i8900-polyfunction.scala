object Test {
  def f[F](f: [t] => t => F): Unit = ()

  f([t] => (x: t) => x)
}
