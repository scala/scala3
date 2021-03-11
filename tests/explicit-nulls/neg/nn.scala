// `.nn` extension method only strips away the outer Null.

class Test {
  val s1: String | Null = ???
  val s2: String = s1 // error
  val s3: String = s1.nn

  val l1: Int = s1.length // error
  val l2: Int = s1.nn.length

  val ss1: Array[String | Null] | Null = ???
  val ss2: Array[String | Null] = ss1.nn
  val ss3: Array[String] = ss1.nn // error
  val ss4: Array[String] = ss1.asInstanceOf

  val a1: String | Null = ss1(0) // error
  val a2: String | Null = ss1.nn(0)
}