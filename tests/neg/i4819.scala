trait One[X]  {
  def concat(suffix: Int): X = ???
}

trait Two[Y <: Foo] {
  def concat[Dummy](suffix: Int): Y = ???
}

class Foo extends One[Foo] with Two[Foo] {
  concat(0) // OK

  // TODO: This does not typecheck because the polymorphic overload is masked
  // (we merge the denotations for both overloads into one and always prefer
  // MethodType to PolyType, instead we should return a MultiDenotation). See #4819.
  concat[Int](0) // error (that should actually not be an error)
}
