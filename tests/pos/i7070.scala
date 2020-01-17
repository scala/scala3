object Test {

  class C

  def (str: String).foo: C ?=> Int = ???

  given C = ???

  val strFoo = "".foo
}