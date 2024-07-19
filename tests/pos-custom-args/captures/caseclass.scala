class C extends caps.Capability
object test1:
  case class Ref(x: String^)

  def test(c: C) =
    val x1 = Ref("hello")
    val y = x1 match
      case Ref(z) => z
    val yc: String = y

object test2:
  case class Ref(x: () => Unit)
  def test(c: C) =

    val pure: () -> Unit = () => ()
    val impure: () => Unit = pure
    val mixed: () ->{c} Unit = pure
    val x = Ref(impure)
    val y0 = x.copy(pure)
    val yc0: Ref = y0

    val x2 = Ref(pure)
    val _: Ref = x2
    val y2 = x2.copy()
    val yc2: Ref = y2

    val x3 = Ref(mixed)
    val _: Ref^{c} = x3
    val y3 = x3.copy()
    val yc3: Ref^{c} = y3

    val y4 = y3 match
      case Ref(xx) => xx
    val y4c: () ->{y3} Unit = y4
