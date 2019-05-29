class Test { // error: class Test cannot be instantiated

  opaque type FlagSet = Int

  opaque type Flag <: FlagSet = String  // error: type String outside bounds  <: Test.this.FlagSet

  object Flag {
    def make(s: String): Flag = s
  }

  val f: Flag = Flag.make("hello")
  val g: FlagSet = f

}