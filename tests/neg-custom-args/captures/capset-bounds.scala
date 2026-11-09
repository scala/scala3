import caps.*

class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y

class Abs[A](val fst: A^, val snd: A^)

def mkAbs[A <: Any^{}](consume x: A^, consume y: A^): Abs[A]^ = Abs[A](x, y)

def Test =
    val r6 = Ref()
    val r7 = Ref()
    val z2 = mkAbs[Ref^{r6}](r6, r7)  // error
    val z3 = mkAbs[Ref](r6, r7)     // error

