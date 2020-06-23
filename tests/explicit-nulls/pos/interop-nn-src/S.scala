class S {
  val j = new J()
  // Test that the `nn` extension method can be used to strip away
  // nullability from a type.
  val s: String = j.foo.nn
  val a: Array[String | Null] = j.bar.nn

  // We can also call .nn on non-nullable types.
  val x: String = ???
  val y: String = x.nn

  // And on other Scala code.
  val x2: String | Null = null
  val y2: String = x2.nn
}
