import caps.Mutable
import caps.any

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y

class Pair(val fst: Ref^, val snd: Ref^)

def mkPair(x: Ref^) = Pair(x, x) // error: separation failure

def bad: Pair^ =
  val r0 = Ref()
  val r1 = mkPair(r0)
  r1

def twoRefs(): Pair^ =
  val r1 = Ref()
  val r2 = Ref()
  Pair(r1, r2)

def good1(): Unit =
  val two = twoRefs()
  val fst = two.fst
  val snd = two.snd
  val twoCopy = Pair(fst, snd) // ok

def good2(): Unit =
  val two = twoRefs()
  val twoCopy = Pair(two.fst, two.snd) // ok

def bad2(): Unit =
  val two = twoRefs()
  val twoCopy = Pair(two.fst, two.fst) // error

class PairPair(val fst: Ref^, val snd: Pair^)

def bad3(): Unit =
  val two = twoRefs()
  val twisted = PairPair(two.fst, two) // error

def swapped(consume x: Pair^, consume y: Ref^): PairPair^ = PairPair(y, x)

def bad4(): Unit =
  val two = twoRefs()
  val twisted = swapped(two, two.snd) // error

def good3(): Unit =
  val two = twoRefs()
  val twoCopy = Pair(two.fst, two.snd)
  val _: Pair^{two.fst, two.snd} = twoCopy
  val twoOther = Pair(two.fst, two.snd)

def bad5(): Unit =
  val two = twoRefs()
  val twoCopy: Pair^ = Pair(two.fst, two.snd)
  val twoOther = Pair(two.fst, two.snd)  // error // error


