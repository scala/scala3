import caps.*
class A extends Mutable:
  var x: Int = 0

def test =
  val a = A()
  val f = () => a.x
  val _: () ->{a.rd} Int = f
  val _: () -> Int = f   // error
  val g = () => a.x += 1
  val _: () ->{a} Unit = g
  val _: () -> Unit = g   // error






