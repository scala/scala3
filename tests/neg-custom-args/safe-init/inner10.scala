object Flags {
  class Inner {
    println(b)            // error

    @scala.annotation.partial
    def foo: Int = 5
  }

  new Flags.Inner         // error
  val b = 5
}
