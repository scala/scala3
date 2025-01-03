
import scala.io.Source

object Lengths:
  opaque type Length = Int
  object Length:
    def apply(i: Int): Length = i
  extension (source: Source)
    def take(length: Length): IndexedSeq[Char] = // no warn
      source.take(length).to(IndexedSeq)
end Lengths

trait Taken:
  def take(n: Lengths.Length): Taken = ???

object Lengthy:
  import Lengths.*
  extension (taken: Taken) def take(n: Length): Taken = ??? // warn

@main def test() = println:
  import Lengths.*
  val src = Source.fromString("hello, world")
  val len = Length("hello".length)
  src.take(len)
