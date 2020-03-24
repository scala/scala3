object MyBoooleanUnapply:
  inline def unapply(x: Int): Boolean = true

object MyOptionUnapply:
  inline def unapply(x: Int): Option[Long] = Some(x)

object MyUnapplyImplicits:
  inline def unapply(x: Int)(using DummyImplicit): Option[Long] = Some(x)

object MyPolyUnapply:
  inline def unapply[T](x: T): Option[T] = Some(x)

object MySeqUnapply:
  inline def unapplySeq(x: Int): Seq[Int] = Seq(x, x)

object MyWhiteboxUnapply:
  transparent inline def unapply(x: Int): Option[Any] = Some(x)

def test: Unit =
  val x = 5 match
    case MyBoooleanUnapply() =>
    case MyOptionUnapply(y) => y: Long
    case MyUnapplyImplicits(y) => y: Long
    case MyPolyUnapply(a) => a: Int
    case MySeqUnapply(a, b) => (a: Int, b: Int)
    case MyWhiteboxUnapply(x) => x: Int
