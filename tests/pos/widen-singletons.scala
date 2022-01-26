object Test:
  def is2(x: 2) = true

  def testValType() =
    val x: 2 = 2
    val v = x
    is2(v)

  def testDefReturnType() =
    def f(): 2 = 2
    val v = f()
    is2(v)
