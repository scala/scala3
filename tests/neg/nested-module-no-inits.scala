//> using options -language:experimental.erasedDefinitions

object Outer:
  object Inner:
    println("effect")

// TODO: this should be pure, see #26800 #24990 #26578
erased val impureOuter: Outer.type = Outer // error
erased val impureInner: Outer.Inner.type = Outer.Inner // error
