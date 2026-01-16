
val x: Mop[Int, Int] = Mop() // error // error

def f() =
  import Predef.{Map as _, *}
  val y: Mop[Int, Int] = Mop() // error // error