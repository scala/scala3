@annotation.capability class C
def test(c: C) =
  val pure: () -> Unit = () => ()
  val impure: () => Unit = pure
  val mixed: {c} () -> Unit = pure
  val x = Ref(impure)
  val _: Ref = x // error
  val y = x.copy()
  val yc: Ref = y // error
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



