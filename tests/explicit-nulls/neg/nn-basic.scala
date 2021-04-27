class Test {
  val s: String | Null = ???
  val s2: String = s.trim // error
  val s3: String | Null = s.nn.trim
  val l: Int = s.length // error
}