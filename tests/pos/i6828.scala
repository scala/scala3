class Foo {
  inline def foo[T](implicit ct: =>scala.reflect.ClassTag[T]): Unit = Unit
  foo[Int]
  foo[String]
}
