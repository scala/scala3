object Flags {
  class Inner {
    println(b)            // error
  }

  new Flags.Inner         // error
  val b = 5
}
