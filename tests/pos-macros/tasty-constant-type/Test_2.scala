object Test {
  val ret = Macro.ff[1, 2]()
  val x: ret.Out = 3
  val y: 3 = compiletime.constValue[ret.Out]

  assert(compiletime.constValue[ret.Out] == 3)
}
