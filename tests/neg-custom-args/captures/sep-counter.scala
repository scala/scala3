import caps.Mutable
import caps.any

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y

class Pair[+X, +Y](val fst: X, val snd: Y)

def test() =
  def mkCounter(): Pair[Ref^, Ref^] =  // error
    val c = Ref()
    val p: Pair[Ref^{c}, Ref^{c}] = Pair(c, c)
    //val q: Pair[Ref^, Ref^] = p
    p
