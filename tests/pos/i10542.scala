package test_10542:

  trait Foo {
    inline def foo[A](t: => A): Unit = ()
  }

  object Bar extends Foo

  object Test {
    Bar.foo {
      sealed trait T1
      case object S1 extends T1
    }
  }

package test_10540:

  trait Foo {
    inline def foo[A](t: => A): Unit = ()
  }

  object Bar extends Foo

  object Test {
    Bar.foo {
      trait T1
      val array = Array(new T1 {})
    }
  }

package test_9655:

  inline def foo[T](inline body: T): T = body

  def test = foo {
    sealed trait Status
    object Active extends Status
  }
