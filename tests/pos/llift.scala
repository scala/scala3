class A {
  class B {
    def outer(): Unit = {
      def inner(): Int = 2

      val fi: Function0[Int] = () => inner()
    }
  }
}
