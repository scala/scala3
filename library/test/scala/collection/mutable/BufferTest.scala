package scala.collection.mutable

import org.junit.Test
import org.junit.Assert.*

import tools.AssertUtil.assertThrows

class BufferTest {

  @Test
  def factoryCreatesArrayDeque: Unit = {
    assertTrue(Buffer(1, 2, 3).isInstanceOf[ArrayDeque[?]])
    assertTrue(IndexedBuffer(1, 2, 3).isInstanceOf[ArrayDeque[?]])
  }

  @Test
  def removeHead: Unit = {
    val buf = Buffer(1, 2, 3)
    assertEquals(1, buf.removeHead())
    assertEquals(Seq(2, 3), buf)
    assertThrows[NoSuchElementException](Buffer[Int]().removeHead())
  }

  @Test
  def removeLast: Unit = {
    val buf = Buffer(1, 2, 3)
    assertEquals(3, buf.removeLast())
    assertEquals(Seq(1, 2), buf)
    assertThrows[NoSuchElementException](Buffer[Int]().removeLast())
  }

  @Test
  def removeHeadOption: Unit = {
    val buf = Buffer(1, 2, 3)
    assertEquals(Some(1), buf.removeHeadOption())
    assertEquals(Seq(2, 3), buf)
    assertEquals(None, Buffer[Int]().removeHeadOption())
  }

  @Test
  def removeLastOption: Unit = {
    val buf = Buffer(1, 2, 3)
    assertEquals(Some(3), buf.removeLastOption())
    assertEquals(Seq(1, 2), buf)
    assertEquals(None, Buffer[Int]().removeLastOption())
  }

  @Test
  def removeAll: Unit = {
    val buf = Buffer(1, 2, 3)
    assertEquals(Seq(1, 2, 3), buf.removeAll())
    assertTrue(buf.isEmpty)
  }

  @Test
  def removeAllReverse: Unit = {
    val buf = Buffer(1, 2, 3)
    assertEquals(Seq(3, 2, 1), buf.removeAllReverse())
    assertTrue(buf.isEmpty)
  }

  @Test
  def removeHeadWhile: Unit = {
    val buf = Buffer(1, 2, 3, 4, 5)
    assertEquals(Seq(1, 2, 3), buf.removeHeadWhile(_ < 4))
    assertEquals(Seq(4, 5), buf)

    val unchanged = Buffer(5, 6, 7)
    assertEquals(Seq(), unchanged.removeHeadWhile(_ < 4))
    assertEquals(Seq(5, 6, 7), unchanged)
  }

  @Test
  def removeLastWhile: Unit = {
    val buf = Buffer(1, 2, 3, 4, 5)
    assertEquals(Seq(5, 4, 3), buf.removeLastWhile(_ > 2))
    assertEquals(Seq(1, 2), buf)

    val unchanged = Buffer(1, 2, 3)
    assertEquals(Seq(), unchanged.removeLastWhile(_ > 10))
    assertEquals(Seq(1, 2, 3), unchanged)
  }

  @Test
  def removeFirst: Unit = {
    val buf = Buffer(1, 2, 3, 4, 5)
    assertEquals(Some(2), buf.removeFirst(_ % 2 == 0))
    assertEquals(Seq(1, 3, 4, 5), buf)

    val fromIndex = Buffer(1, 2, 3, 4, 5)
    assertEquals(Some(4), fromIndex.removeFirst(_ % 2 == 0, 2))
    assertEquals(Seq(1, 2, 3, 5), fromIndex)

    val notFound = Buffer(1, 3, 5)
    assertEquals(None, notFound.removeFirst(_ % 2 == 0))
    assertEquals(Seq(1, 3, 5), notFound)
  }

  @Test
  def removeAllMatching: Unit = {
    val buf = Buffer(1, 2, 3, 4, 5)
    assertEquals(Seq(2, 4), buf.removeAll(_ % 2 == 0))
    assertEquals(Seq(1, 3, 5), buf)

    val noneMatch = Buffer(1, 3, 5)
    assertEquals(Seq(), noneMatch.removeAll(_ % 2 == 0))
    assertEquals(Seq(1, 3, 5), noneMatch)

    val allMatch = Buffer(2, 4, 6)
    assertEquals(Seq(2, 4, 6), allMatch.removeAll(_ % 2 == 0))
    assertTrue(allMatch.isEmpty)
  }

  // `ArrayDeque` overrides these, so test the default implementations on other buffers
  @Test
  def removeAllDefaults: Unit =
    for (mk <- Seq[Seq[Int] => Buffer[Int]](ArrayBuffer.from, ListBuffer.from)) {
      val all = mk(Seq(1, 2, 3))
      assertEquals(Seq(1, 2, 3), all.removeAll())
      assertTrue(all.isEmpty)

      val reversed = mk(Seq(1, 2, 3))
      assertEquals(Seq(3, 2, 1), reversed.removeAllReverse())
      assertTrue(reversed.isEmpty)

      val matching = mk(Seq(1, 2, 3, 4, 5))
      assertEquals(Seq(2, 4), matching.removeAll(_ % 2 == 0))
      assertEquals(Seq(1, 3, 5), matching)

      val noneMatch = mk(Seq(1, 3, 5))
      assertEquals(Seq(), noneMatch.removeAll(_ % 2 == 0))
      assertEquals(Seq(1, 3, 5), noneMatch)
    }
}
