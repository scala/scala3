class C extends caps.SharedCapability
object test2:
  case class Ref(x: () => Unit)
  def test(c: C) =

    val y3: Ref{val x: () ->{c} Unit}^{c} = ???

    val y4 = y3 match
      case Ref(xx) => xx
    val y4c: () ->{c} Unit = y4
    //val y4d: () ->{y3.x} Unit = y4
