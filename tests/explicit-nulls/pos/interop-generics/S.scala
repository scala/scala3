class ReturnedFromJava[T] {}

class S {
  val j = new J()
  // Check that the inside of a Java generic isn't nullified
  val i: I[String]|Null = j.foo("hello")
}
