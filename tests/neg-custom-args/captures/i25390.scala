import language.experimental.safe

def main =
  val col = scala.collection.mutable.Buffer(23) // error
  col(0) = 47
  assert(col.sameElements(List(47)))

