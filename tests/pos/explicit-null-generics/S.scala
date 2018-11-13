class ReturnedFromJava[T] {}

class S {
  val j = new J()
  // Check that the inside of a Java generic isn't nullified
  val i: I[String]|Null = j.foo("hello")
  // ... but if the generic is Scala-defined and used from Java, then the |Null _is_ propagated to the inside.
  val fromJava: ReturnedFromJava[String|Null]|Null = j.foo2("hello")
}
