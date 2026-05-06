import caps.Mutable
import caps.any

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y

class Pair[+X, +Y](val fst: X, val snd: Y)

def mkPair[X](x: X): Pair[X, X] = Pair(x, x)

def bad: Pair[Ref^, Ref^] =  // error: overlap at r1*, r0
  val r0 = Ref()
  val r1: Pair[Ref^, Ref^] = mkPair(r0) // error: overlap at r0
  r1

class SamePair[+X](val fst: X, val snd: X)

def twoRefs(): Pair[Ref^, Ref^] =
  val r1 = Ref()
  val r2 = Ref()
  Pair(r1, r2)

def twoRefs2(): SamePair[Ref^] =
  val r1 = Ref()
  val r2 = Ref()
  val r3: SamePair[Ref^] = SamePair(r1, r1) // ok
  r3

def twoRefsBad(): Pair[Ref^, Ref^] =
  Pair(Ref(), Ref()) // ok now

val io: Object^ = ???

def test(): Unit =
  val two = twoRefs()
  val fst: Ref^{two.fst*} = two.fst
  val snd: Ref^{two.snd*} = two.snd
  val twoCopy: Pair[Ref^, Ref^] = Pair(fst, snd) // ok

  val same = twoRefs2()
  val fstSame = same.fst
  val sndSame = same.snd
  val sameToPair: Pair[Ref^, Ref^] = Pair(fstSame, sndSame) // error



