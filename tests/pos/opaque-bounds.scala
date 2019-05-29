object Test {


  opaque type FlagSet = Int

  opaque type Flag <: FlagSet = Int

  object Flag {
    def make(n: Int): Flag = n
  }

  val f: Flag = Flag.make(1)
  val g: FlagSet = f

}