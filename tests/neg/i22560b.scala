
class Enumeration:
  protected class Val(i: Int):
    def this() = this(42)
  object Val

class Test extends Enumeration:
  val Hearts = Val(27) // error
  val Diamonds = Val() // error
