case class ShortString(value: String)

class StringOps(x: ShortString):
  def *(n: Int): ShortString = ???
  def capitalize: ShortString = ???

object stringsyntax:

  extension (x: ShortString)
    def take(n: Int): ShortString = ???
    def drop(n: Int): ShortString = ???
    private def moreOps = StringOps(x)
    export moreOps.*
