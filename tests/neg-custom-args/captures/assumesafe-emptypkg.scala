import caps.*
class Text

object Console extends SharedCapability:
  def println(s: String): Unit = ()

@assumeSafe object A:
  def println(s: String) = Console.println(s) // error

