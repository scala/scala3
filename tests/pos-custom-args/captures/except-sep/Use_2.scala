def test(f: File^) =
  runOnNewThread: () =>
    f.read()
  val r = restricted(f)
  val ro = readOnlyPart(f)
