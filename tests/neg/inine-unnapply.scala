
object Foo {
  inline def unapply(x: Any): Boolean = ??? // error: Implementation restriction: inline unapply methods are not supported
  inline def unapplySeq(x: Any): Seq[Any] = ??? // error: Implementation restriction: inline unapplySeq methods are not supported
}
