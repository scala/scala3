class JavaVarargs_1 {
  static void method(String... args) {}

  static Object multiple(Object first, String... others) {
    return String.valueOf(first) + others.length;
  }
}
