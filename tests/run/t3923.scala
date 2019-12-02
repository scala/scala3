object Test {
  def main(args: Array[String]): Unit = {
    assert(collection.mutable.ArraySeq[Int]() == Nil)
    assert(collection.mutable.ArraySeq[Int]() == Seq())
    assert(Seq[Int]() == collection.mutable.ArraySeq[Int]())
    assert(Nil == collection.mutable.ArraySeq[Int]())
  }
}
