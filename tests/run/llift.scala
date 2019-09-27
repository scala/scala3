class A {
  class B {
    def outer(): Unit = {
      def inner(): Int = 2

      val fi: Function0[Int] = () => inner()
    }
  }
}

object Test {
  def foo(x: Int) = {
    trait B {
      def bar = x
    }
    class C extends B {
      override def bar = super[B].bar
    }
    new C().bar
  }

  def f1(x: Int) = {
    class C1 {
      def f2 = {
        class C2 {
          def f3 = {
            def f4 = x
            f4
          }
        }
        new C2().f3
      }
    }
    new C1().f2
  }

  def f1a(x: Int) = {
//    class C1 {
      def f2 = {
        trait T2 {
          def f3 = {
            def f4 = x
            def f5 = f4
            f5
          }
        }
        class C2 extends T2
        new C2().f3
      }
//    }
      /*new C1().*/f2
  }

  def f1b(x: Int) = {
    class T1 {
      def f2 = {
        trait T2 {
          def f3 = {
            def f4 = x
            def f5 = f4
            f5
          }
        }
        class C2 extends T2
        new C2().f3
      }
    }
    class C1 extends T1
    new C1().f2
  }

  def f1c(x: Int) = {
    class T1 {
      def f2 = {
        trait T2 {
          def f3: Int = {
            def f4 = x
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
      def f6 = x
    }
    class C1 extends T1
    new C1().f2
  }

  def f1d(x: Int) = {
    trait T1 {
      def f2 = {
        trait T2 {
          def f3: Int = {
            def f4 = x
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
      def f6 = x
    }
    class C1 extends T1
    new C1().f2
  }

  def f1e(x: Int) = {
    trait T1 {
      def f2 = {
        trait T2 {
          def f3: Int = x
        }
        class C2 extends T2
        new C2().f3
      }
      def f6 = x
    }
    class C1 extends T1
    new C1().f6
  }

  def f1f(x: Int) = {
    trait T1 {
      trait T2 {
        def f3: Int = x
      }
      class C2 extends T2 {
        override def f3 = super.f3
      }
      new C2().f3
      def f6 = x
    }
    class C1 extends T1
    new C1().f6
  }

  def main(args: Array[String]) = {
    assert(foo(3) == 3)
    assert(f1(4) == 4)
    assert(f1a(5) == 5)
    assert(f1b(6) == 6)
    assert(f1c(7) == 14)
    assert(f1d(8) == 16)
    assert(f1e(9) == 9)
    assert(f1f(10) == 10)
  }
}
