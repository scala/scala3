object TestRuntimeCompat  {
  def main(args: Array[String]): Unit =  {
    // unoptimized
    ArraysOpsTests()
    RangeTests()
    EnumerationTests()

    // optimized
    scala.collection.immutable.test.ReadBlackTreeTests()
    OrderingTests()
    MurmurHash3Tests()
  }
}
