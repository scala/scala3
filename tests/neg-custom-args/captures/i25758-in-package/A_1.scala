package ttt
import caps.*
class Text

object Console extends SharedCapability:
  def println(s: String): Unit = ()

object A uses Console:
  def println(s: String) = Console.println(s)

