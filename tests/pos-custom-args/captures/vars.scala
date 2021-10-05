class CC
type Cap = {*} CC

def test(cap1: Cap, cap2: Cap) =
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  var x = f
  val y = x
  val z = () => if x("") == "" then "a" else "b"
  val zc: {cap1} () => String = z
  val z2 = () => { x = identity }
  val z2c: {cap1} () => Unit = z2

  class Ref:
    var elem: {cap1} String => String = null

  val r = Ref()
  r.elem = f
  val fc: {cap1} String => String = r.elem
