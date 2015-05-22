




object Test {

  def main(args: Array[String]): Unit = {
    testConversions()
    testConverters()
  }

  def needPackageConcurrentMap(map: collection.concurrent.Map[Int, Int]): Unit = {
  }
  def needJavaConcurrent(map: java.util.concurrent.ConcurrentMap[Int, Int]): Unit = {
  }

  def testConversions(): Unit = {
    import collection.JavaConversions._
    val skiplist = new java.util.concurrent.ConcurrentSkipListMap[Int, Int]
    val ctrie = new collection.concurrent.TrieMap[Int, Int]

    needPackageConcurrentMap(skiplist)
    needJavaConcurrent(ctrie)
  }

  def testConverters(): Unit = {
    import collection.JavaConverters._
    val skiplist = new java.util.concurrent.ConcurrentSkipListMap[Int, Int]
    val ctrie = new collection.concurrent.TrieMap[Int, Int]

    needPackageConcurrentMap(skiplist.asScala)
    needJavaConcurrent(ctrie.asJava)
  }

}
