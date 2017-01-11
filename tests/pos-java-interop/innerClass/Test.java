public class Test {
  public static void test() {
    Outer outer = new Outer();
    Outer.InnerInClass innerInClass = outer.inner();

    // Does not work yet, requires https://github.com/DarkDimius/scala/pull/4
    // Outer.InnerInObject innerInObject = new Outer.InnerInObject();
  }
}
