def f(x: Int): Int = ???
case class IntBox(x: Int)
case class Box[T](x: T)

def test: Unit =
  val x: Int = ???
  val y: Int = ???
  def g(x: Int): Int = ???

  val v1: {v: Int with v == 1} = 2 // error
  val v2: {v: Int with v == x} = y // error
  val v3: {v: Int with v == x + 1} = x + 2 // error
  val v4: {v: Int with v == f(x)} = g(x) // error
  val v5: {v: Int with v == g(x)} = f(x) // error
  val v6: {v: IntBox with v == IntBox(x)} = IntBox(x + 1) // error
  val v7: {v: Box[Int] with v == Box(x)} = Box(x + 1) // error
  val v8: {v: Int with v == x + f(x)} = x + g(x) // error
  val v9: {v: Int with v == x + g(x)} = x + f(x) // error
  val v10: {v: Int with v == f(x + 1)} = f(x + 2) // error
  val v11: {v: Int with v == g(x + 1)} = g(x + 2) // error
  val v12: {v: IntBox with v == IntBox(x + 1)} = IntBox(x) // error
  val v13: {v: Box[Int] with v == Box(x + 1)} = Box(x) // error
