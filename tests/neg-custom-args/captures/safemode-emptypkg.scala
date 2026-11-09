import language.experimental.safe
import caps.*
class Text

object Console extends SharedCapability:
  def println(s: String): Unit = ()

object A:
  def println(s: String) = Console.println(s) // error

