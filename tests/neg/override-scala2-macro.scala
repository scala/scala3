class Foo extends StringContext {
  override inline def f[A >: Any](args: A*): String = ??? // error
}
