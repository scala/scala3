// Modified from #19662 to make it independent of the cc stdlib.
import language.experimental.captureChecking
trait MySeq[+A] { self: MySeq[A]^ =>
  def map[B](f: A => B): MySeq[B]^{this, f}
}
object MySeq:
  def apply(x: Int): MySeq[Int] = ???
class C:
  private def foo = MySeq(5).map { i => i }
