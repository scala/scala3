abstract class Test { // error: class Test cannot be instantiated since it has a member Flag with possibly conflicting bounds
  opaque type FlagSet = Int

  opaque type Flag <: FlagSet = String

  object Flag {
    def make(s: String): Flag = s
  }

  val f: Flag = Flag.make("hello")
  val g: FlagSet = f

}