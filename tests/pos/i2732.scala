object Foo {
  // Example 1
  val fun: java.util.function.Function[String, _ <: String] = x => x

  // Example 2
  val map = new java.util.HashMap[String, String]
  map.computeIfAbsent("hello", foo => "world")
}
