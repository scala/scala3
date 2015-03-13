object Test {
  class Foo[T]
  type C[T] = Foo[_ <: T]   // error: parameter type T of type alias does not appear as type argument of the aliased class Foo
  val a: C[AnyRef] = new Foo[AnyRef]
}
