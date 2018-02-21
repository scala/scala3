object opaquetypes {
  opaque val x: Int = 1 // error

  opaque class Foo // error

  opaque type T // error

  opaque type U <: String // error

  opaque type O = String

  val s: O = "" // error

  object O {
    val s: O = "" // should be OK
  }

}

