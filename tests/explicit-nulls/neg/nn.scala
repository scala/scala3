// `.nn` extension method only strips away the outer Null.

class Test {
  val s1: String | Null = ???
  val s2: String = s1.nn

  val ss1: Array[String | Null] | Null = ???
  val ss2: Array[String | Null] = ss1.nn
  val ss3: Array[String] = ss1.nn // error
}