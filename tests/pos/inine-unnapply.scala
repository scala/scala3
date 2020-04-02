
object Foo {
  inline def unapply(x: Any): Boolean = ???
  inline def unapplySeq(x: Any): Seq[Any] = ???
}
