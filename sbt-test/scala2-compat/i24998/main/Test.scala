object TestRuntimeCompat  {
  def main(args: Array[String]): Unit =  {
    // Failed in unoptimized
    ArraysOpsTests()
    RangeTests()
    EnumerationTests()

    // Failed only in optimized and heavily inlined
    scala.collection.immutable.test.ReadBlackTreeTests()
    OrderingTests()
    MurmurHash3Tests()
  }
}
