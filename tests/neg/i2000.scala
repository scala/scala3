object test1 {
  class C[A] { def foo(a: A) = "c" }
  class D extends C[String] { override def foo(implicit s: String) = "d" } // error
}

object test2 {
  class C[A] { final def foo(a: A) = "c" }
  class D extends C[String] { def foo(implicit s: String) = "d" } // error
  object Test {
    def main(args: Array[String]) =
      new D
  }
}

object test3 {
  class A {
    def foo(implicit i: Int): Int = i + i
  }

  class B extends A {
    override def foo(i: Int): Int = i // error
  }
}
