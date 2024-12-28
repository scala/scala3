
//> using options -Wunused:imports

object MyImplicits:
  extension (a: Int) def print: Unit = println(s"Hello, I am $a")

import MyImplicits.print //Global import of extension
object Foo:
  def printInt(a: Int): Unit = a.print
  import MyImplicits.* // warn //Local import of extension
