@annotation.capability class Cap

object Monomorphic2:

  class Pair(x: Cap => Unit, y: Cap => Unit):
    def fst: Cap^{cap[Pair]} ->{x} Unit = x  // error
    def snd: Cap^{cap[Pair]} ->{y} Unit = y  // error

  def test(c: Cap, d: Cap) =
    def f(x: Cap): Unit = if c == x then ()
    def g(x: Cap): Unit = if d == x then ()
    val p = Pair(f, g)
    val x1 = p.fst
    val x1c: Cap ->{c} Unit = x1  // error
    val y1 = p.snd
    val y1c: Cap ->{d} Unit = y1  // error

