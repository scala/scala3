object TestSafeValues {
  val HashCodeLength = 32
  val BitPartitionSize = 5
  val MaxDepth = HashCodeLength.toDouble
}

object A { // These are a safe values, so no warning should be emitted
    TestSafeValues.HashCodeLength 
    TestSafeValues.BitPartitionSize
    TestSafeValues.MaxDepth 
}