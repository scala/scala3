class X extends scala.reflect.Selectable:
  def plus = "1"

@main def Test =
  val x = X()
  assert(x.selectDynamic("plus") == "1")
  assert(x.applyDynamic("plus")() == "1")