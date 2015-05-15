import Macros._

object Test extends dotty.runtime.LegacyApp {
  List(1, 2) match { case UnapplyMacro(x, y) => println((x, y)) }
  List(1, 2, 3) match { case UnapplyMacro(x, y, z) => println((x, y, z)) }
}