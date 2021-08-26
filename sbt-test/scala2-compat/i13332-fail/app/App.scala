import collectionstrawman.*

import scala.deriving.Mirror

def syntheticSumMirror = {
  val m = new ListModule()

  val mIList = summon[Mirror.Of[m.IList[Int]]] // error: can't summon
  type derivedICons = Tuple.Head[mIList.MirroredElemTypes]
  val mICons = summon[Mirror.Of[derivedICons]] // error: can't summon
}

def syntheticProductMirror = {
  val m = new ListModule()

  val mIPair = summon[Mirror.Of[m.IPair[Int, String]]] // error: can't summon
}
