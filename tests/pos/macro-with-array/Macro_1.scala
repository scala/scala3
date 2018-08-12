
object Macro {

  rewrite def foo0(i: Int): Unit = ~{ '() }
  rewrite def foo1(arr: Array[Boolean]): Unit = ~{ '() }
  rewrite def foo2(arr: Array[Byte]): Unit = ~{ '() }
  rewrite def foo3(arr: Array[Short]): Unit = ~{ '() }
  rewrite def foo4(arr: Array[Int]): Unit = ~{ '() }
  rewrite def foo5(arr: Array[Long]): Unit = ~{ '() }
  rewrite def foo6(arr: Array[Float]): Unit = ~{ '() }
  rewrite def foo7(arr: Array[Double]): Unit = ~{ '() }
  rewrite def foo8(arr: Array[Char]): Unit = ~{ '() }
  rewrite def foo9(arr: Array[Object]): Unit = ~{ '() }
  rewrite def foo10(arr: Array[String]): Unit = ~{ '() }
  rewrite def foo11[T](arr: Array[T]): Unit = ~{ '() }
  rewrite def foo12(arr: Array[Array[Int]]): Unit = ~{ '() }
  rewrite def foo13(arr: Array[Array[String]]): Unit = ~{ '() }
  rewrite def foo14(arr: Array[Array[Array[Int]]]): Unit = ~{ '() }
  rewrite def foo15(arr: Array[Any]): Unit = ~{ '() }
  rewrite def foo16(arr: Array[AnyVal]): Unit = ~{ '() }
  rewrite def foo17(arr: Array[AnyRef]): Unit = ~{ '() }
  rewrite def foo18(arr: Array[Foo]): Unit = ~{ '() }
  rewrite def foo19(arr: Array[Macro.type]): Unit = ~{ '() }
  rewrite def foo20(arr: Array[Bar]): Unit = ~{ '() }
  rewrite def foo21(arr: Array[Baz.type]): Unit = ~{ '() }
  rewrite def foo22(arr: Array[Foo#A]): Unit = ~{ '() }

  class Bar
  object Baz
}

class Foo {
  class A
}
