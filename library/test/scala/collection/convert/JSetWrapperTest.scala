package scala.collection.convert

import java.{util => ju}

import org.junit.Assert.*
import org.junit.Test
import scala.collection.Set

@deprecated("Tests deprecated API", since="2.13")
class JSetWrapperTest {

  import scala.collection.JavaConverters.*

  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jSet: ju.Set[Int] = new ju.HashSet(ju.Arrays.asList(1, 2, 3))
    val sSet: Set[Int] = jSet.asScala

    assertTrue(sSet.isInstanceOf[JavaCollectionWrappers.JSetWrapper[?]])
    assertTrue(sSet.iterator.sameElements(Set(1, 2, 3)))
  }

  @Test
  def testFilterInPlace(): Unit = {
    val jSet: ju.Set[Int] = new ju.HashSet(ju.Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    val sSet: collection.mutable.Set[Int] = jSet.asScala

    sSet.filterInPlace(_ % 2 == 0)

    assertEquals(sSet, Set(2, 4, 6, 8, 10))
  }

}
