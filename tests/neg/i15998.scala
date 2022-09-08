import scala.collection.SeqOps

trait ComparingOps:
  extension[A, CC[B] <: SeqOps[B, CC, CC[B]]](ring: CC[A])
    def isRotationOf(that: CC[A]): Boolean = ???

object RingSeq extends ComparingOps
import RingSeq.*

@main def Test =
  RingSeq.isRotationOf("DAB") // error
  "ABCD".isRotationOf("DAB")  // error

  // workaround
  RingSeq.isRotationOf[Char, IndexedSeq]("DAB")
  RingSeq.isRotationOf(wrapString("DAB"))
  wrapString("ABCD").isRotationOf("DAB")

  def foo[X](using x: X): X = x

  val x = foo  // error
