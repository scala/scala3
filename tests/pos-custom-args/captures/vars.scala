class Cap extends caps.Capability

def test(cap1: Cap, cap2: Cap) =
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  var x = f
  val y = x
  val z = () => if x("") == "" then "a" else "b"
  val zc: () ->{cap1} String = z
  val z2 = () => { x = identity }
  val z2c: () ->{cap1} Unit = z2

  class Ref:
    var elem: String ->{cap1} String = null

  val r = Ref()
  r.elem = f
  val fc: String ->{cap1} String = r.elem