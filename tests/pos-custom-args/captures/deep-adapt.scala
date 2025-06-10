import caps.Mutable
import caps.cap

trait Rdr[T]:
  def get: T

class Ref[T](init: T) extends Rdr[T], Mutable:
  private var current = init
  def get: T = current
  update def put(x: T): Unit = current = x

case class Pair[+A, +B](x: A, y: B)
class Swap[+A, +B](x: A, y: B) extends Pair[B, A](y, x)

def Test(c: Object^): Unit =
  val refs = List(Ref(1), Ref(2))
  val rdrs: List[Ref[Int]^{cap.rd}] = refs
  val rdrs2: Seq[Ref[Int]^{cap.rd}] = refs

  val swapped = Swap(Ref(1), Ref("hello"))
  val _: Swap[Ref[Int]^{cap.rd}, Ref[String]^{cap.rd}] = swapped
  val _: Pair[Ref[String]^{cap.rd}, Ref[Int]^{cap.rd}] @unchecked = swapped

