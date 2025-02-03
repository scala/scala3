import caps.Mutable
import caps.cap

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  mut def put(y: Int): Unit = x = y

case class Pair[+X, +Y](val fst: X, val snd: Y)

def twoRefs(): Pair[Ref^, Ref^] =
  val r1 = Ref()
  val r2 = Ref()
  Pair(r1, r2)

def twoRefsBad(): Pair[Ref^, Ref^] =
  Pair(Ref(), Ref()) // error: universal capability cannot be included in capture set
                     // even though this is morally equivalent to `twoRefs`


def test(io: Object^): Unit =
  val two = twoRefs()
  val fst = two.fst // error: local reach capability two* leaks into test
                    // first, the leakage makes no sense
                    // second, the capture should be two.fst, not two*
  val snd = two.snd
  val three: Pair[Ref^{io}, Ref^{io}] = ???
  val bad = three.fst
