import caps.Mutable
import caps.any

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
  val rdrs: List[Ref[Int]^{any.rd}] = refs
  val rdrs2: Seq[Ref[Int]^{any.rd}] = refs

  val swapped = Swap(Ref(1), Ref("hello"))
  val _: Swap[Ref[Int]^{any.rd}, Ref[String]^{any.rd}] = swapped
  val _: Pair[Ref[String]^{any.rd}, Ref[Int]^{any.rd}] @unchecked = swapped

