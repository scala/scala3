class A {
  final def foo(g: () => Int) = {
    class B {
      def m: Int = g()
      lazy val b = new B
    }

    new B
  }

  final def bar() = {
    class B {
      def m: Int = 100
      lazy val b = new B
    }

    new B
  }

  println(foo(() => f))    // error
  println(bar())           // ok

  def f: Int = 10
}