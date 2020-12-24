package test1:

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
/*
package test2:

  trait Foo {
    inline def foo[A](t: => A): Unit = ()
  }

  object Bar extends Foo

  object Test {
    Bar.foo {
      trait T1
      val array = Array(new T1 {})
    }
  }*/