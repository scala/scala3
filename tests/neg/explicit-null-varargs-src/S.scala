class S {

  val j2 = new J2()
  val x: Array[Dog] = ???
  j2.foo(x: _*)

  //val j = new J()
  //val x: Array[String] = ???
  //j.foo(x: _*) // error: expected Array[String|Null] but got Array[String]
  
  //val x2: Array[String|Null] = ???
  //j.foo(x2: _*) // ok
  // j.foo(null: _*) // ok
}
