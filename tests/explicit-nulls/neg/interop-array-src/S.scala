class S {

  val j = new J()
  val x: Array[String] = ???
  j.foo(x) // error: expected Array[String|Null] but got Array[String]
  
  val x2: Array[String|Null] = ???
  j.foo(x2) // ok
  j.foo(null) // ok
}
