// Test is pending because we have no good way to test it.
// We need to: Compile Fork.java, and then compile Test.scala
// with Fork.class on the classpath.
public @interface Fork_1 {
  int value() default -1;
  String[] jvmArgs() default { "nope" };
}
