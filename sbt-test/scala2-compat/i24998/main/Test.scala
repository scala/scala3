object TestRuntimeCompat  {
  def main(args: Array[String]): Unit =  {
    ArraysOpsTests()
    RangeTests()
    scala.collection.immutable.test.ReadBlackTreeTests()
    EnumerationTests()
    OrderingTests()
    MurmurHash3Tests()
  }
}
