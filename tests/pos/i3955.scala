import language.experimental.namedTypeArguments
object Foo {
  inline def f[S, T](x: S): T = ???
  def g(x: Int) = f[T = Any](x)
}
