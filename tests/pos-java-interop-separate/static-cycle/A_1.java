class Foo<T extends Foo<T>> {}

public class A_1 extends Foo<A_1.InnerClass> {
  public static class InnerClass extends Foo<A_1.InnerClass> {}
}
