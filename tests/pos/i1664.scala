object test {
  def f[a](x: a) = {
    def print = x
    class A {
      def f() = {
        class B { def h = print }
        new B
      }
      f()
    }
  }
}
