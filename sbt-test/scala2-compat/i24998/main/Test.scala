object TestRuntimeCompat  {
  def main(args: Array[String]): Unit =  {
    // Failed in unoptimized
    ArraysOpsTests()
    RangeTests()
    EnumerationTests()
    MurmurHash3Tests()

    // Failed only in optimized and heavily inlined
    scala.collection.immutable.test.ReadBlackTreeTests()
    OrderingTests()
  }
}
