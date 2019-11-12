// Test that `JavaNull`able unions are transparent
// w.r.t member selections.

class Test {
  val s: String|JavaNull = "hello"
  val l: Int = s.length // ok: `JavaNull` allows (unsound) member selections.

  val s2: JavaNull|String = "world"
  val l2: Int = s2.length

  val s3: JavaNull|String|JavaNull = "hello"
  val l3: Int = s3.length

  val s4: (String|JavaNull)&(JavaNull|String) = "hello"
  val l4 = s4.length
}
