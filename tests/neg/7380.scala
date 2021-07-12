import scala.deriving.Mirror

object Test {
  summon[Mirror.Of[(Int, String)] {
    type MirroredElemTypes = (Int, Int, Int)
  }] // error
  // MirroredElemTypes missmatch, expected: (Int, String), found: (Int, Int, Int).

  summon[Mirror.Of[(Int, String)] {
    type MirroredElemLabels = ("_1", "_2", "_3")
  }]  // error
  // MirroredElemLabels missmatch, expected: (("_1" : String), ("_2" : String)),
  // found: (("_1" : String), ("_2" : String), ("_3" : String)).
}
