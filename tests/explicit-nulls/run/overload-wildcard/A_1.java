public class A_1 {
  // Overloads whose formal parameter types become flexible under explicit nulls.
  public static <T> int foo5(Class<? extends T> a) { return 1; }
  public static <T> int foo5(Class<T> a, int... ints) { return 2; }

  public static <V> int foo10(java.util.Map<String, ? extends V> a) { return 1; }
  public static <V> int foo10(java.util.Map<String, V> a, int... ints) { return 2; }

  // Returns a wildcard type, so the argument type is flexible at the call site.
  public static Class<? extends Object> cls() { return Object.class; }

  public static java.util.Map<String, ? extends Object> map() {
    return new java.util.HashMap<String, Object>();
  }
}
