def test(): Unit =
  val fun = ??? : (() -> Object^)
  val l = fun()  // Since `fun` is pure we get `l: Object` with the APPLY rule
  val _: Object = l

