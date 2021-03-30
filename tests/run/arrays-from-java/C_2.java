import java.io.Serializable;

class B extends A {
  @Override
  public <T extends Serializable> void foo1(T[] x) {}
  @Override
  public <T extends Object & Serializable> void foo2(T[] x) {}
}

public class C_2 {
  public static void test() {
    A a = new A();
    B b = new B();
    String[] arr = { "" };
    a.foo1(arr);
    a.foo2(arr);

    b.foo1(arr);
    b.foo2(arr);
  }
}
