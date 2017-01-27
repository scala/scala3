public class Test {
  public static void test() {
    Outer outer = new Outer();
    Outer.InnerInClass innerInClass = outer.inner();

    Outer.InnerInObject innerInObject = new Outer.InnerInObject();
  }
}
