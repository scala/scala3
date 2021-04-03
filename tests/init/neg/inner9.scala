object Flags {
  class Inner {
    println(b)
  }

  new Flags.Inner

  val b = 5              // error
}

object Flags2 {
  class Inner {
    println(b)
  }


  lazy val a = 3
  val b = 5
}
