class C
type Cap = {*} C

class Pair[+A, +B](x: A, y: B):
    def fst: A = x
    def snd: B = y

def test(c: Cap) =
  def f(x: Cap): Unit = if c == x then ()
  val p = Pair(f, f)
  val g = () => p.fst == p.snd
  val gc: () -> Boolean = g  // error

