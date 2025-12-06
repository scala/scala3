class A_1 {
  public static int foo1(Object x) { return 1; }
  public static int foo1(String... x) { return 2; }

  public static int foo2(Object x) { return 1; }
  public static int foo2(Object... x) { return 2; }

  public static <T> int foo3(T x) { return 1; }
  public static <T> int foo3(T... x) { return 2; }

  public static <T> int foo4(T x) { return 1; }
  public static <T> int foo4(T x, T... y) { return 2; }

  // https://github.com/scala/scala3/issues/24072
  public static <T> int foo5(Class<? extends T> a) { return 1; }
  public static <T> int foo5(Class<T> a, int... ints) { return 2; }

  public static <T> int foo6(Class<T> a, int... ints) { return 1; }
  public static <T> int foo6(int a) { return 2; }

  public static <T extends Number> int foo7(Class<? extends T> a) { return 1; }
  public static <T extends Number> int foo7(Class<T> a, int... ints) { return 2; }

  public static <T extends Number> int foo8(Class<? extends T> a) { return 1; } // (a)
  public static <T> int foo8(Class<T> a, int... ints) { return 2; } // (b)

  public static <T> int foo9(Class<? extends T> a) { return 1; } // (a)
  public static <T extends Number> int foo9(Class<T> a, int... ints) { return 2; } // (b)

  public static boolean check() {
    // Java prefers non-varargs to varargs:
    // https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.12.2
    return
        foo1("") == 1 &&
        foo2("") == 1 &&
        foo3("") == 1 &&
        foo4("") == 1 &&
        foo5(Object.class) == 1 &&
        foo6(Object.class) == 1 &&
        foo7(Integer.class) == 1 &&
        foo8(Integer.class) == 1 &&
        foo9(Integer.class) == 1;
  }
}
