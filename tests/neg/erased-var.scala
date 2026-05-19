//> using options -language:experimental.erasedDefinitions

case class Ev(i: Int)

object A:
  var ev = Ev(0)
  erased val m: Unit = {
    ev = Ev(1)
  }
