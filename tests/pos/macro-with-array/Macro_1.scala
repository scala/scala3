
object Macro {

  transparent def foo0(i: Int): Unit = ~{ '() }
  transparent def foo1(arr: Array[Boolean]): Unit = ~{ '() }
  transparent def foo2(arr: Array[Byte]): Unit = ~{ '() }
  transparent def foo3(arr: Array[Short]): Unit = ~{ '() }
  transparent def foo4(arr: Array[Int]): Unit = ~{ '() }
  transparent def foo5(arr: Array[Long]): Unit = ~{ '() }
  transparent def foo6(arr: Array[Float]): Unit = ~{ '() }
  transparent def foo7(arr: Array[Double]): Unit = ~{ '() }
  transparent def foo8(arr: Array[Char]): Unit = ~{ '() }
  transparent def foo9(arr: Array[Object]): Unit = ~{ '() }
  transparent def foo10(arr: Array[String]): Unit = ~{ '() }
  transparent def foo11[T](arr: Array[T]): Unit = ~{ '() }
  transparent def foo12(arr: Array[Array[Int]]): Unit = ~{ '() }
  transparent def foo13(arr: Array[Array[String]]): Unit = ~{ '() }
  transparent def foo14(arr: Array[Array[Array[Int]]]): Unit = ~{ '() }
  transparent def foo15(arr: Array[Any]): Unit = ~{ '() }
  transparent def foo16(arr: Array[AnyVal]): Unit = ~{ '() }
  transparent def foo17(arr: Array[AnyRef]): Unit = ~{ '() }
  transparent def foo18(arr: Array[Foo]): Unit = ~{ '() }
  transparent def foo19(arr: Array[Macro.type]): Unit = ~{ '() }
  transparent def foo20(arr: Array[Bar]): Unit = ~{ '() }
  transparent def foo21(arr: Array[Baz.type]): Unit = ~{ '() }
  transparent def foo22(arr: Array[Foo#A]): Unit = ~{ '() }

  class Bar
  object Baz
}

class Foo {
  class A
}
