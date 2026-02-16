def test(c: Object^) =
  val x: () -> Unit = caps.unsafe.unsafeDiscardUses(() => println(c))
