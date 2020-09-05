class A_1 {
  public static int foo1(Object x) { return 1; }
  public static int foo1(String... x) { return 2; }

  public static int foo2(Object x) { return 1; }
  public static int foo2(Object... x) { return 2; }

  public static <T> int foo3(T x) { return 1; }
  public static <T> int foo3(T... x) { return 2; }

  public static <T> int foo4(T x) { return 1; }
  public static <T> int foo4(T x, T... y) { return 2; }

  public static boolean check() {
    // Java prefers non-varargs to varargs:
    // https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.12.2
    return
        foo1("") == 1 &&
        foo2("") == 1 &&
        foo3("") == 1 &&
        foo4("") == 1;
  }
}
