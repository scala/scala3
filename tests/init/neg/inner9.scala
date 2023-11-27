class Outer:
  val flags = new Flags // error

  class Flags {
    class Inner {
      println(b)
    }

    new flags.Inner

    val a = this.b + 3
    val b = 5 // error
  }

  class Flags2 {
    class Inner {
      println(b)
    }


    lazy val a = 3
    val b = 5
  }
