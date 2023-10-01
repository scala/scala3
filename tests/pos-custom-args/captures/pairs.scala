//class CC
//type Cap = CC^
@annotation.capability class Cap

object Generic:

  class Pair[+A, +B](x: A, y: B):
    def fst: A = x
    def snd: B = y

  def test(c: Cap, d: Cap) =
    def f(x: Cap): Unit = if c == x then ()
    def g(x: Cap): Unit = if d == x then ()
    val p = Pair(f, g)
    val x1 = p.fst
    val x1c: Cap^{cap[test]} ->{c} Unit = x1
    val y1 = p.snd
    val y1c: Cap^{cap[test]} ->{d} Unit = y1

object Monomorphic:

  class Pair(x: Cap => Unit, y: Cap => Unit):
    type PCap = Cap
    def fst: PCap ->{x} Unit = x
    def snd: PCap ->{y} Unit = y

  def test(c: Cap, d: Cap) =
    def f(x: Cap): Unit = if c == x then ()
    def g(x: Cap): Unit = if d == x then ()
    val p = Pair(f, g)
    val x1 = p.fst
    val x1c: Cap ->{c} Unit = x1
    val y1 = p.snd
    val y1c: Cap ->{d} Unit = y1

object Monomorphic2:

  class Pair(x: Cap => Unit, y: Cap => Unit):
    def fst: Cap^{cap[Pair]} ->{x} Unit = x
    def snd: Cap^{cap[Pair]} ->{y} Unit = y

  class Pair2(x: Cap => Unit, y: Cap => Unit):
    def fst: Cap^{cap[Pair2]} => Unit = x
    def snd: Cap^{cap[Pair2]} => Unit = y

  def test(c: Cap, d: Cap) =
    def f(x: Cap): Unit = if c == x then ()
    def g(x: Cap): Unit = if d == x then ()
    val p = Pair(f, g)
    val x1 = p.fst
    val x1c: Cap ->{c} Unit = x1
    val y1 = p.snd
    val y1c: Cap ->{d} Unit = y1

