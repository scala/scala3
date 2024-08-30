trait Service {
  def method: String
}

object MySuite {
  def foo(a: List[String]) = ???
  def foo(a: String) = ???

  foo { // error

    new Service {
      private val underlying: Service = ???
      private val s = "foo"

      export underlying.*
      export s.toLowerCase
    }

    ???
  }
}
