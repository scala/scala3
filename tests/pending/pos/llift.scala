object Test {
  def f1d(x: Int) = {
    trait T1 {
//      def f2 = {
        trait T2 {
          def f3: Int = x
        }
        class C2 extends T2
        new C2().f3
//      }
      def f6 = x
    }
    class C1 extends T1
    new C1().f6
  }
}
