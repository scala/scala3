class Foo {
  inline def foo[T](implicit ct: => scala.reflect.ClassTag[T]): Unit = ()
  foo[Int]
  foo[String]
}
