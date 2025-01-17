//class CC
//type Cap = CC^
class Cap extends caps.Capability

object Generic:

  class Pair[+A, +B](x: A, y: B):
    def fst: A = x
    def snd: B = y

  def test(c: Cap, d: Cap) =
    def f(x: Cap): Unit = if c == x then ()
    def g(x: Cap): Unit = if d == x then ()
    val p = Pair(f, g)
    val x1 = p.fst
    val x1c: Cap ->{c} Unit = x1
    val y1 = p.snd
    val y1c: Cap ->{d} Unit = y1
