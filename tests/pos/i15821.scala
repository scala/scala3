def main =
  foo.bar(42)
  foo.bar

package object foo {
  def bar[F[_]]: Unit = ???
  def bar[F[_]](x: Int): Unit = ???
  private[foo] def bar[F[_]](x: Int)(implicit dummy: DummyImplicit): Unit = ???
}
