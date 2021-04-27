class S {
  val j = new J()
  val x = j.foo()
  // Check that the type of `x` is inferred to be `String|Null`.
  // i.e. the union isn't collapsed.
  val y: String | Null = x
}
