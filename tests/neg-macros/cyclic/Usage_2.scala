package macros_usage

import macros.* // error

case class Container(a: Int):
  def store(b: Int): String = s"storing $a and $b"

object MacroReflection:
  @main
  def main(args: String*): Unit =
    val tuple = createTuple[3, String]("Scala")
    println(tuple)
