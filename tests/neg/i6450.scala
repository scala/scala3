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


object Test1 {
  trait A {
    def apply(x: Integer): A
  }
  object B {
    def f(e: String): A = ???
    val f: A = ???
    val g: A = f(null)  // error: ambiguous
  }
}

object Test2 {
  trait A {
    def apply(x: String): A = {println(2); null}
  }

  object B {
    def f(e: Integer): A = {println(1); null}
    val f: A = f(null)   // error: ambiguous
  }
}