

object RangeTests {
  def apply(): Unit = {
    checkSingleElemIterator(Range(0, 1).iterator)
    checkSingleElemIterator(Range(0, 1).grouped(1))
    checkSingleElemIterator(Range(0, 0).inits)
    checkSingleElemIterator(Range(0, 0).tails)
    assert(Range(0, 0).grouped(1).isEmpty)
  }

  private def checkSingleElemIterator(it: Iterator[Any]): Unit = {
    println(s"Check iterator: ${it.getClass()}")
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