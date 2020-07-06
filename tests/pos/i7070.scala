object Test {

  class C

  extension (str: String) def foo: C ?=> Int = ???

  given C = ???

  val strFoo = "".foo
}