import caps.Mutable
import caps.any

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y

class Pair[+X, +Y](val fst: X, val snd: Y)

def twoRefs1(): Pair[Ref^, Ref^] =
  Pair(Ref(), Ref())

def twoRefs2(): Pair[Ref^, Ref^] =
  val p = Pair(Ref(), Ref())
  Pair(p.fst, p.snd)

def twoRefs3(): Pair[Ref^, Ref^] = // error but should work
  val p = Pair(Ref(), Ref())
  p

