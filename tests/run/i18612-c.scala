class X extends scala.reflect.Selectable:
  def + = "1"

@main def Test =
  val x = X()
  assert(x.selectDynamic("+") == "1")
  assert(x.applyDynamic("+")() == "1")