trait A {
  def apply(x: Any): Int =  1
}

object B {
  def f(x: Any): Int = 2
  lazy val f = new A {}
  val x = f(null)  // error: ambiguous
}

object Test {
  def f(x: Any) = 10
  val f: Foo123 = f(1)  // error: Not found: type Foo123

}

object Test2 {
  trait A {
    def apply(x: AnyRef): Int
  }
  object B {
    def f(e: Any): A = ???
    val f: A = f(null)
  }
}

object Test3 {
  trait A {
    def apply(x: String): Int
  }
  object B {
    def f(e: CharSequence): A = ???
    val f: A = f(null)
  }
}


object Test4 {
  trait A {
    def apply(x: Any): Int
  }
  object B {
    def f(e: Any): A = ???
    val f: A = f(null)
  }
}

object Test5 {
  trait A {
    def apply(x: Any): Int
  }
  object B {
    def f(e: AnyRef): A = ???
    val f: A = f(null)
  }
}


object Test6 {
  trait A {
    def apply(x: CharSequence): Int
  }
  object B {
    def f(e: String): A = ???
    val f: A = f(null)
  }
}