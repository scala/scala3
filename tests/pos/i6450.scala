class Foo {
  def A(i: Double) = i
  object A {
    def apply(i: Int): Int = i+1
  }
  def B(i: Any) = i
  object B {
    def apply(i: Int) = i+1
    def apply(b: Float) = true
  }
  def C(i: Int) = i + 1
  object C {
    def apply(i: Double): Double = i
  }
  def foo = A(0)
  def bar = B(1)
  def baz = C(2)
}

object Test {
  val x = new Foo().foo
  val y = new Foo().bar
  val z = new Foo().baz
  val x1: Int = x
  val y1: Int = y
  val z1: Int = z
}

object Test1 {

  trait A {
    def apply(x: Any): Int =  1
  }

  object B {
    def f(x: Any): A = new A {}
    lazy val f: A = f(null)
  }
}

object Test2 {
  trait A {
    def apply(x: AnyRef): Int
  }
  object B {
    def f(e: Any): A = ???
    val f: A = ???
    val g: A = f(null)
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
