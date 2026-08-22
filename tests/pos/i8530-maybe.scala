//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*
object MyBoooleanUnapply:
  inline def unapply(x: Int): Boolean = true

object MyOptionUnapply:
  inline def unapply(x: Int): Long? = x

object MyUnapplyImplicits:
  inline def unapply(x: Int)(using DummyImplicit): Long? = x

object MyPolyUnapply:
  inline def unapply[T](x: T): T? = Ok(x)

object MySeqUnapply:
  inline def unapplySeq(x: Int): Seq[Int] = Seq(x, x)

object MyWhiteboxUnapply:
  transparent inline def unapply(x: Int): Any? = Ok(x)

def test: Unit =
  val x = 5 match
    case MyBoooleanUnapply() =>
    case MyOptionUnapply(y) => y: Long
    case MyUnapplyImplicits(y) => y: Long
    case MyPolyUnapply(a) => a: Int
    case MySeqUnapply(a, b) => (a: Int, b: Int)
    case MyWhiteboxUnapply(x) => x: Int
