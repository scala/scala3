package scala.collection

import org.junit.Assert.assertEquals
import org.junit.Test
import scala.collection.immutable.Vector

class StrictOptimizedSeqTest {

  @Test
  def hasCorrectDistinct(): Unit = {
    assertEquals(Vector(1, 2, 3, 4, 5), Vector(1, 1, 2, 3, 3, 3, 4, 5, 5).distinct)
  }

  @Test
  def hasCorrectDistinctBy(): Unit = {
    val result = Vector("a", "aa", "aaa", "b", "bb", "bbb", "bbbb", "c").distinctBy(_.length)

    assertEquals(Vector("a", "aa", "aaa", "bbbb"), result)
  }
}
