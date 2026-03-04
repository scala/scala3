class C extends caps.SharedCapability
def test(c: C) =
  val pure: () -> Unit = () => ()
  val impure: () => Unit = pure
  val mixed: () ->{c} Unit = pure
  val x = Ref(impure)
  val _: Ref = x // error
  val y = caps.unsafe.unsafeAssumeSeparate(x.copy()) // TODO remove
  val yc: Ref = y // error
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
  val y4c: () ->{y3} Unit = y4 // was error, now OK
