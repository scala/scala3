class S {
  val j = new J()
  // Test that the `nn` extension method can be used to strip away
  // nullability from a type.
  val s: String = j.foo.nn
  val a: Array[String|Null] = j.bar.nn
}
