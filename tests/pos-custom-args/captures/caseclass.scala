@annotation.capability class C
object test1:
  case class Ref(x: {*} String)

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
    val mixed: {c} () -> Unit = pure
    val x = Ref(impure)
    val y0 = x.copy(pure)
    val yc0: Ref = y0

    val x2 = Ref(pure)
    val _: Ref = x2
    val y2 = x2.copy()
    val yc2: Ref = y2

    val x3 = Ref(mixed)
    val _: {c} Ref = x3
    val y3 = x3.copy()
    val yc3: {c} Ref = y3

    val y4 = y3 match
      case Ref(xx) => xx
    val y4c: {x3} () -> Unit = y4
