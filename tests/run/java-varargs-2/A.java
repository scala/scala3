class A {
  public static void foo(int... args) {
  }

  public static <T> void gen(T... args) {
  }

  public static <T extends java.io.Serializable> void gen2(T... args) {
  }
}
