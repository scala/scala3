object Test {

  class C

  def (str: String) foo: given C => Int = ???

  given as C = ???

  val strFoo = "".foo
}