
object Macro {

  inline def foo0(i: Int): Unit = ${ '{} }
  inline def foo1(arr: Array[Boolean]): Unit = ${ '{} }
  inline def foo2(arr: Array[Byte]): Unit = ${ '{} }
  inline def foo3(arr: Array[Short]): Unit = ${ '{} }
  inline def foo4(arr: Array[Int]): Unit = ${ '{} }
  inline def foo5(arr: Array[Long]): Unit = ${ '{} }
  inline def foo6(arr: Array[Float]): Unit = ${ '{} }
  inline def foo7(arr: Array[Double]): Unit = ${ '{} }
  inline def foo8(arr: Array[Char]): Unit = ${ '{} }
  inline def foo9(arr: Array[Object]): Unit = ${ '{} }
  inline def foo10(arr: Array[String]): Unit = ${ '{} }
  inline def foo11[T](arr: Array[T]): Unit = ${ '{} }
  inline def foo12(arr: Array[Array[Int]]): Unit = ${ '{} }
  inline def foo13(arr: Array[Array[String]]): Unit = ${ '{} }
  inline def foo14(arr: Array[Array[Array[Int]]]): Unit = ${ '{} }
  inline def foo15(arr: Array[Any]): Unit = ${ '{} }
  inline def foo16(arr: Array[AnyVal]): Unit = ${ '{} }
  inline def foo17(arr: Array[AnyRef]): Unit = ${ '{} }
  inline def foo18(arr: Array[Foo]): Unit = ${ '{} }
  inline def foo19(arr: Array[Macro.type]): Unit = ${ '{} }
  inline def foo20(arr: Array[Bar]): Unit = ${ '{} }
  inline def foo21(arr: Array[Baz.type]): Unit = ${ '{} }
  inline def foo22(arr: Array[Foo#A]): Unit = ${ '{} }

  class Bar
  object Baz
}

class Foo {
  class A
}
