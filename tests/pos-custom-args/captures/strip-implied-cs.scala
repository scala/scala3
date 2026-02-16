//> using options -Werror
import caps.*

trait State[A] extends SharedCapability:
  def get: A
  def set(a: A): Unit

def get[A]: State[A] ?-> A          = s ?=> s.get
def set[A](a: A): State[A] ?-> Unit = s ?=> s.set(a)

trait Rand extends SharedCapability:
  def range(min: Int, max: Int): Int

object Rand:
  def fromState: (s: State[Long]) ?-> Rand^{s} =
    new Rand:
      override def range(min: Int, max: Int): Int =
        val seed             = get
        val (nextSeed, next) = (seed + 1, seed.toInt) // obviously wrong, but not the point...
        set(nextSeed)
        next
