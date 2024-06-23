class X extends scala.reflect.Selectable:
  val `+` = "1"

@main def Test =
  val x = X()
  assert(x.selectDynamic("+") == "1")