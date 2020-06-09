class A {
  def bn(x: => Any): Any = x
  def foo: Unit = {
    bn({
      class A
      def foo(x: A): Unit = {}
      foo(new A)
    })
  }
}
