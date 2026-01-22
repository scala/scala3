// Test for new ArrayDeque-style APIs on Buffer
object Test {
  def main(args: Array[String]): Unit = {
    testRemoveHead()
    testRemoveLast()
    testRemoveHeadOption()
    testRemoveLastOption()
    testRemoveAll()
    testRemoveAllReverse()
    testRemoveHeadWhile()
    testRemoveLastWhile()
    testRemoveFirst()
    testRemoveAllPredicate()
    println("All tests passed!")
  }

  def testRemoveHead(): Unit = {
    import scala.collection.mutable.Buffer
    val buf = Buffer(1, 2, 3)
    val head = buf.removeHead()
    assert(head == 1, s"removeHead should return 1, got $head")
    assert(buf.toSeq == Seq(2, 3), s"buffer should be Seq(2, 3), got ${buf.toSeq}")

    // Test empty buffer throws
    val emptyBuf = Buffer[Int]()
    var threw = false
    try {
      emptyBuf.removeHead()
    } catch {
      case _: NoSuchElementException => threw = true
    }
    assert(threw, "removeHead on empty buffer should throw NoSuchElementException")
    println("testRemoveHead passed")
  }

  def testRemoveLast(): Unit = {
    import scala.collection.mutable.Buffer
    val buf = Buffer(1, 2, 3)
    val last = buf.removeLast()
    assert(last == 3, s"removeLast should return 3, got $last")
    assert(buf.toSeq == Seq(1, 2), s"buffer should be Seq(1, 2), got ${buf.toSeq}")

    // Test empty buffer throws
    val emptyBuf = Buffer[Int]()
    var threw = false
    try {
      emptyBuf.removeLast()
    } catch {
      case _: NoSuchElementException => threw = true
    }
    assert(threw, "removeLast on empty buffer should throw NoSuchElementException")
    println("testRemoveLast passed")
  }

  def testRemoveHeadOption(): Unit = {
    import scala.collection.mutable.Buffer
    val buf = Buffer(1, 2, 3)
    val head = buf.removeHeadOption()
    assert(head == Some(1), s"removeHeadOption should return Some(1), got $head")
    assert(buf.toSeq == Seq(2, 3), s"buffer should be Seq(2, 3), got ${buf.toSeq}")

    val emptyBuf = Buffer[Int]()
    val emptyHead = emptyBuf.removeHeadOption()
    assert(emptyHead == None, s"removeHeadOption on empty buffer should return None, got $emptyHead")
    println("testRemoveHeadOption passed")
  }

  def testRemoveLastOption(): Unit = {
    import scala.collection.mutable.Buffer
    val buf = Buffer(1, 2, 3)
    val last = buf.removeLastOption()
    assert(last == Some(3), s"removeLastOption should return Some(3), got $last")
    assert(buf.toSeq == Seq(1, 2), s"buffer should be Seq(1, 2), got ${buf.toSeq}")

    val emptyBuf = Buffer[Int]()
    val emptyLast = emptyBuf.removeLastOption()
    assert(emptyLast == None, s"removeLastOption on empty buffer should return None, got $emptyLast")
    println("testRemoveLastOption passed")
  }

  def testRemoveAll(): Unit = {
    import scala.collection.mutable.Buffer
    val buf = Buffer(1, 2, 3)
    val removed = buf.removeAll()
    assert(removed == Seq(1, 2, 3), s"removeAll should return Seq(1, 2, 3), got $removed")
    assert(buf.isEmpty, s"buffer should be empty after removeAll, got ${buf.toSeq}")
    println("testRemoveAll passed")
  }

  def testRemoveAllReverse(): Unit = {
    import scala.collection.mutable.Buffer
    val buf = Buffer(1, 2, 3)
    val removed = buf.removeAllReverse()
    assert(removed == Seq(3, 2, 1), s"removeAllReverse should return Seq(3, 2, 1), got $removed")
    assert(buf.isEmpty, s"buffer should be empty after removeAllReverse, got ${buf.toSeq}")
    println("testRemoveAllReverse passed")
  }

  def testRemoveHeadWhile(): Unit = {
    import scala.collection.mutable.Buffer
    val buf = Buffer(1, 2, 3, 4, 5)
    val removed = buf.removeHeadWhile(_ < 4)
    assert(removed == Seq(1, 2, 3), s"removeHeadWhile should return Seq(1, 2, 3), got $removed")
    assert(buf.toSeq == Seq(4, 5), s"buffer should be Seq(4, 5), got ${buf.toSeq}")

    // Test with empty result
    val buf2 = Buffer(5, 6, 7)
    val removed2 = buf2.removeHeadWhile(_ < 4)
    assert(removed2 == Seq(), s"removeHeadWhile should return empty Seq, got $removed2")
    assert(buf2.toSeq == Seq(5, 6, 7), s"buffer should be unchanged, got ${buf2.toSeq}")
    println("testRemoveHeadWhile passed")
  }

  def testRemoveLastWhile(): Unit = {
    import scala.collection.mutable.Buffer
    val buf = Buffer(1, 2, 3, 4, 5)
    val removed = buf.removeLastWhile(_ > 2)
    assert(removed == Seq(5, 4, 3), s"removeLastWhile should return Seq(5, 4, 3), got $removed")
    assert(buf.toSeq == Seq(1, 2), s"buffer should be Seq(1, 2), got ${buf.toSeq}")

    // Test with empty result
    val buf2 = Buffer(1, 2, 3)
    val removed2 = buf2.removeLastWhile(_ > 10)
    assert(removed2 == Seq(), s"removeLastWhile should return empty Seq, got $removed2")
    assert(buf2.toSeq == Seq(1, 2, 3), s"buffer should be unchanged, got ${buf2.toSeq}")
    println("testRemoveLastWhile passed")
  }

  def testRemoveFirst(): Unit = {
    import scala.collection.mutable.Buffer
    val buf = Buffer(1, 2, 3, 4, 5)
    val removed = buf.removeFirst(_ % 2 == 0)
    assert(removed == Some(2), s"removeFirst should return Some(2), got $removed")
    assert(buf.toSeq == Seq(1, 3, 4, 5), s"buffer should be Seq(1, 3, 4, 5), got ${buf.toSeq}")

    // Test with from parameter
    val buf2 = Buffer(1, 2, 3, 4, 5)
    val removed2 = buf2.removeFirst(_ % 2 == 0, 2)
    assert(removed2 == Some(4), s"removeFirst(from=2) should return Some(4), got $removed2")
    assert(buf2.toSeq == Seq(1, 2, 3, 5), s"buffer should be Seq(1, 2, 3, 5), got ${buf2.toSeq}")

    // Test not found
    val buf3 = Buffer(1, 3, 5)
    val removed3 = buf3.removeFirst(_ % 2 == 0)
    assert(removed3 == None, s"removeFirst should return None, got $removed3")
    assert(buf3.toSeq == Seq(1, 3, 5), s"buffer should be unchanged, got ${buf3.toSeq}")
    println("testRemoveFirst passed")
  }

  def testRemoveAllPredicate(): Unit = {
    import scala.collection.mutable.Buffer
    val buf = Buffer(1, 2, 3, 4, 5)
    val removed = buf.removeAll(_ % 2 == 0)
    assert(removed == Seq(2, 4), s"removeAll predicate should return Seq(2, 4), got $removed")
    assert(buf.toSeq == Seq(1, 3, 5), s"buffer should be Seq(1, 3, 5), got ${buf.toSeq}")

    // Test with none removed
    val buf2 = Buffer(1, 3, 5)
    val removed2 = buf2.removeAll(_ % 2 == 0)
    assert(removed2 == Seq(), s"removeAll predicate should return empty Seq, got $removed2")
    assert(buf2.toSeq == Seq(1, 3, 5), s"buffer should be unchanged, got ${buf2.toSeq}")

    // Test with all removed
    val buf3 = Buffer(2, 4, 6)
    val removed3 = buf3.removeAll(_ % 2 == 0)
    assert(removed3 == Seq(2, 4, 6), s"removeAll predicate should return Seq(2, 4, 6), got $removed3")
    assert(buf3.isEmpty, s"buffer should be empty, got ${buf3.toSeq}")
    println("testRemoveAllPredicate passed")
  }
}
