object Test {
  def f1c(x: Int) = {
    class T1 {
      def f2 = {
        trait T2 {
          def f3: Int = {
            def f4 = 10
            def f5 = f4
            def f7 = this.f3
            f5
          }
          def f3a = f3
        }
        class C2 extends T2
        class C3 extends T1
        new C2().f3a + new C3().f6
      }
      def f6 = 10
    }
  }
}