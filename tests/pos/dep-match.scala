object Test {
  type Foo = Int { type U }
  type Bar[T] = T match {
    case Unit => Unit
  }
  inline def baz(foo: Foo): Unit = {
    val v: Bar[foo.U] = ???
  }
}
