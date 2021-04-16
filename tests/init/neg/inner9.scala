object Flags {     // error
  class Inner {
    println(b)
  }

  new Flags.Inner

  val b = 5
}

object Flags2 {
  class Inner {
    println(b)
  }


  lazy val a = 3
  val b = 5
}
