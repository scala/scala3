//> using options -experimental -language:experimental.namedTuples
import language.experimental.namedTuples

val directionsNT = IArray(
  (dx = 0, dy = 1), // up
  (dx = 1, dy = 0), // right
  (dx = 0, dy = -1), // down
  (dx = -1, dy = 0), // left
)
val IArray(UpNT @ _, _, _, _) = directionsNT

object NT:
  def foo[T <: (x: Int, y: String)](tup: T): Int =
    tup.x

  def union[T](tup: (x: Int, y: String) | (x: Int, y: String)): Int =
    tup.x

  def intersect[T](tup: (x: Int, y: String) & T): Int =
    tup.x


@main def Test =
  println(UpNT.dx)
  println(NT.union((1, "a")))
  println(NT.intersect((2, "b")))
