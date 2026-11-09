//> using options -language:experimental.erasedDefinitions

case class Ev(i: Int)

object A:
  var ev = Ev(0)
  erased val m: Unit = { // error
    ev = Ev(1)
  }
