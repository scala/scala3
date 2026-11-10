package pkg;

// Package-private, like the part classes of Kotlin `@JvmMultifileClass` facades
class Impl extends Base {
  public static final Object SENTINEL = "sentinel";
  public static int counter = 0;
  public static Object getSentinel() { return SENTINEL; }
  public static String hidden() { return "Impl.hidden"; }
  public static String over(int x) { return "Impl.over(int)"; }
}
