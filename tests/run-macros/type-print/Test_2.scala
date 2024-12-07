import scala.compiletime.ops.int.*

inline def printAll[T]: Unit =
  println(printTypeShort[T])
  println(printType[T])
  println(printTypeAnsi[T])
  println(printTypeStructure[T])

@main
def Test: Unit =
  printAll[List[Int]]
  val a = 1
  val b = 2
  printAll[3 + a.type * b.type]
  printAll[(3 + a.type) * b.type]
