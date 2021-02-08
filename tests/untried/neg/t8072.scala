class NoIfParSeq {
  import collection.parallel.*
  val x = List(1,2)
  val y = x.ifParSeq[Int](throw new Exception).otherwise(0)  // Shouldn't compile
  val z = x.toParArray
}
