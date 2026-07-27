//> using options -language:experimental.erasedDefinitions

object Outer:
  object Inner:
    println("effect")

erased val pureOuter: Outer.type = Outer
erased val impureInner: Outer.Inner.type = Outer.Inner // error
