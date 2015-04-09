object sigs {

  type Lst[A] = List[A]

  type Twin[B] = (B, B)

  var x = 7 * 9

  class Base {

    def foo(x: Int): Any = 33
    def foo: Object = "x"

  }

  class Sub extends Base {

   override def foo = "abc"

   override def foo(x: Int) = "abc"
  }

  trait A { self: B =>
    type AA
    val a: AA & BB

  }

  trait B { this: A =>
    type BB
    val b: AA & BB
  }

  class C extends A with B {
    type AA = String
    type BB = AnyRef
    val a = ""
    val b = ""
  }


}
