class S {
  val s1: String | Null = ???
  val s2: String = ???
  val n: Null = ???
  val ss1: Array[String] = ???
  val ss2: Array[String | Null] = ???

  locally {
    s1 == null
    s1 != null
    null == s1
    null != s1

    s2 == null // error
    s2 != null // error
    null == s2 // error
    null != s2 // error

    s1 == s2
    s1 != s2
    s2 == s1
    s2 != s1

    n == null
    n != null
    null == n
    null != n

    s1 == n
    s2 == n // error
    n != s1
    n != s2 // error
  }

  locally {
    ss1 == null // error
    ss1 != null // error
    null == ss1 // error
    null != ss1 // error

    ss1 == n // error
    ss1 != n // error
    n == ss1 // error
    n != ss1 // error

    ss1 == ss2
    ss2 != ss1
  }
}