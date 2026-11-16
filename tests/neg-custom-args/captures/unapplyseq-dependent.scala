import caps.*

class IO extends SharedCapability

// The element type of the extractor result refers to the scrutinee
object Sq:
  def unapplySeq(x: IO): Option[Seq[() ->{x} Unit]] = None

def test1(io: IO): () -> Unit = io match
  case Sq(f, rest*) => f // error
  case _ => () => ()

def test2(io: IO): () -> Unit = io match
  case Sq(f, rest*) => rest.head // error
  case _ => () => ()

def test3(io: IO): () ->{io} Unit = io match
  case Sq(f, rest*) => f
  case _ => () => ()
