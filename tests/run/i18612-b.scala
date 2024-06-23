class X extends scala.reflect.Selectable:
  val plus = "1"

@main def Test =
  val x = X()
  assert(x.selectDynamic("plus") == "1")