import ChangeVal._

class Bar:
  @change(5)
  val foo: Int = 3

@main def Test =
  val t = new Bar
  assert(t.foo == 5)
