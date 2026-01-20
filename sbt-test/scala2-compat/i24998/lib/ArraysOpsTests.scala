

object ArraysOpsTests {
  def apply(): Unit = {
    testSpecializedIterators()
    testSpecializedReverseIterator()
  }

  def testSpecializedIterators(): Unit = {
    checkSingleElemIterator(Array(false).iterator)
    checkSingleElemIterator(Array(1: Byte).iterator)
    checkSingleElemIterator(Array(1: Char).iterator)
    checkSingleElemIterator(Array(1: Short).iterator)
    checkSingleElemIterator(Array(1: Int).iterator)
    checkSingleElemIterator(Array(1: Long).iterator)
    checkSingleElemIterator(Array(1: Float).iterator)
    checkSingleElemIterator(Array(1: Double).iterator)
    checkSingleElemIterator(Array("AnyRef": AnyRef).iterator)
    checkSingleElemIterator(Array((): Unit).iterator)
  }

  def testSpecializedReverseIterator(): Unit = {
    checkSingleElemIterator(Array(false).reverseIterator)
    checkSingleElemIterator(Array(1: Char).reverseIterator)
    checkSingleElemIterator(Array(1: Byte).reverseIterator)
    checkSingleElemIterator(Array(1: Short).reverseIterator)
    checkSingleElemIterator(Array(1: Int).reverseIterator)
    checkSingleElemIterator(Array(1: Long).reverseIterator)
    checkSingleElemIterator(Array(1: Float).reverseIterator)
    checkSingleElemIterator(Array(1: Double).reverseIterator)
    checkSingleElemIterator(Array("AnyRef").reverseIterator)
    checkSingleElemIterator(Array((): Unit).reverseIterator)
  }

  private def checkSingleElemIterator(it: Iterator[Any]): Unit = {
    println(s"Check ArrayOps iterator: ${it.getClass()}")
    assert(it.hasNext)
    assert(it.next() != null)
    assert(!it.hasNext)
    try it.next().ensuring(false, "should not reach")
    catch { case _: NoSuchElementException => () }
    assert(!it.hasNext)
    try it.next().ensuring(false, "should not reach")
    catch { case _: NoSuchElementException => () }
  }
}
