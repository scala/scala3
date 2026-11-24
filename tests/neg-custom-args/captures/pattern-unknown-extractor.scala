import caps.*

// The extractor result is a generic tuple, so the checker does not know how
// its components are selected. They capture what the result captures.
object Ex:
  def unapplySeq(x: Int): Option[List[() => Unit] *: Seq[Int] *: EmptyTuple] = None

def test(n: Int): List[() -> Unit] = n match
  case Ex(l, rest*) => l // error
  case _ => Nil
