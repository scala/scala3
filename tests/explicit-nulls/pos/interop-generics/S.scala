class S {
  val j = new J()
  // Check that the inside of a Java generic isn't nullified
  val x: I[String] | Null = j.foo("hello")
  val y: Array[I[String] | Null] | Null = j.bar[String](null)
}
