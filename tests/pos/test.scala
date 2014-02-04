object test {
  
  import collection.JavaConverters._
  
  private val elems =
    (new java.util.LinkedHashMap[String, List[Int]]).asScala
  val elems2: collection.mutable.Map[String, List[Int]] = elems
}