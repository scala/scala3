class Foo {
  inline def foo[T](implicit ct: =>scala.reflect.ClassTag[T]): Unit = Unit
  type U
  foo[U] // error
}