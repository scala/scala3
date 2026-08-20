import caps.*

class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y

class Abs[A](val fst: A^, val snd: A^)

def mkAbs[A](consume x: A^, consume y: A^): Abs[A]^ = Abs[A](x, y)
def mkAbs2[A <: Any{}](consume x: A^, consume y: A^): Abs[A]^ = Abs[A](x, y)

def Test =
    val r6 = Ref()
    val r7 = Ref()
    val z = mkAbs(r6, r7)
    val elem5 = z.fst
    val elem6: Ref^ = z.snd
    val z3 = mkAbs2(elem5, elem6)   // ok, can spit and join for polymorphic as well.
