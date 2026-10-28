import language.experimental.erasedDefinitions

class Test:
  val x = (1, Option((erased y: Int))) // error
  val y = (1, erased y: Int) // error
  val z = (erased y: Int) // error

  val x1 = (a: Int, Option((erased y: Int))) => 1 // error
  val y1 = (a: Int, erased y: Int) => 1 // ok
  val z1 = (erased y: Int) => 1 // ok