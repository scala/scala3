opaque type Pos = Double

object Pos:
  extension (x: Pos)
    def mult1(y: Pos): Pos = x * y

extension (x: Pos)
  def mult2(y: Pos): Pos = x * y

class Test:
  def test(key: String, a: Pos, b: Pos): Unit =
    val tup1 = new Tuple1(Pos.mult1(a)(b))
    val res1: Pos = tup1._1

    val tup2 = new Tuple1(a.mult1(b))
    val res2: Pos = tup2._1

    val tup3 = new Tuple1(mult2(a)(b))
    val res3: Pos = tup3._1

    val tup4 = new Tuple1(a.mult2(b))
    val res4: Pos = tup4._1 // was error: Found: (tup4._4 : Double) Required: Pos
