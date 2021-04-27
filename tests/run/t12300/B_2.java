class OneTwo implements One, Two {}
// Java doesn't seen class parents of traits
class YSubX extends Y implements YSub {}

public class B_2 {
  public static void foo() {
    A a = new A();
    a.foo1(new One() {});
    a.foo2(new One() {});
    a.foo3(new OneTwo());
    a.foo4(new OneTwo());
    a.foo5(new Y());
    a.foo6(new Y());
    a.foo7(new YSubX());
    a.foo8(new YSubX());
  }
}
