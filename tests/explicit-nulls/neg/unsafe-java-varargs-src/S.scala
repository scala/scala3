class S {
  val j: J = ???

  j.foo()
  j.foo("")
  j.foo(null)
  j.foo("", "")
  j.foo("", null, "")

  val arg1: Array[String] = ???
  val arg2: Array[String | Null] = ???
  val arg3: Array[String] | Null = ???
  val arg4: Array[String | Null] | Null = ???

  j.foo(arg1: _*)
  j.foo(arg2: _*)
  j.foo(arg3: _*) // error
  j.foo(arg4: _*) // error
}