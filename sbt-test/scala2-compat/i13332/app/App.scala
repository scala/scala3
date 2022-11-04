import collectionstrawman.*

import scala.deriving.Mirror

def syntheticSumMirror = {
  val m = new ListModule()

  val mIList = summon[Mirror.Of[m.IList[Int]]]
  val mICons = summon[Mirror.Of[Tuple.Head[mIList.MirroredElemTypes]]]
}

def syntheticProductMirror = {
  val m = new ListModule()

  val mIPair = summon[Mirror.Of[m.IPair[Int, String]]]
}