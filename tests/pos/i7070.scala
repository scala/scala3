object Test {

  class C

  def (str: String).foo: (given C) => Int = ???

  given C = ???

  val strFoo = "".foo
}