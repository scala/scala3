object i15398 {
  type Tuple23 = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

  summon[Tuple.Size[Tuple23] =:= 23]
  val m = summon[scala.deriving.Mirror.Of[Tuple23]]
  summon[m.MirroredLabel =:= "Tuple"]
  summon[m.MirroredElemTypes =:= Tuple23]
  summon[m.MirroredElemLabels =:= ("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9", "_10", "_11", "_12", "_13", "_14", "_15", "_16", "_17", "_18", "_19", "_20", "_21", "_22", "_23")]
}
