public @interface Fork {
  int value() default -1;
  String[] jvmArgs() default { "nope" };
}
