trait One[X]  {
  def concat(suffix: Int): X = ???
}

trait Two[Y <: Foo] {
  def concat[Dummy](suffix: Int): Y = ???
}

class Foo extends One[Foo] with Two[Foo] {
  concat(0) // error: ambiguous overload
  concat[Int](0) // OK
}
