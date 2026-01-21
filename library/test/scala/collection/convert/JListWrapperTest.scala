package scala.collection.convert

import java.{util => ju}

import org.junit.Assert.assertTrue
import org.junit.Test
import scala.collection.immutable.List
import scala.collection.mutable.Buffer

@deprecated("Tests deprecated API", since="2.13")
class JListWrapperTest {

  import scala.collection.JavaConverters.*

  @Test
  def testIteratorDoesNotCauseStackOverflow(): Unit = {
    val jList: ju.List[Int] = ju.Arrays.asList(1, 2, 3)
    val sList: Buffer[Int] = jList.asScala

    assertTrue(sList.isInstanceOf[JavaCollectionWrappers.JListWrapper[?]])
    assertTrue(sList.iterator.sameElements(List(1, 2, 3)))
  }
}
