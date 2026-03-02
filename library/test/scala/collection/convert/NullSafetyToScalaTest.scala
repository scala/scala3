package scala.collection.convert

import java.util.{concurrent => juc}
import java.{lang => jl, util => ju}

import org.junit.Test
import org.junit.Assert.assertNull

import scala.jdk.CollectionConverters.*

// scala/bug#9113: tests to ensure that wrappers return null instead of wrapping it as a collection

class NullSafetyToScalaTest {
  @Test def testIteratorDecoration(): Unit = {
    val nullJIterator: ju.Iterator[AnyRef] = null.asInstanceOf[ju.Iterator[AnyRef]]

    assertNull(nullJIterator.asScala)
  }

  @Test def testEnumerationDecoration(): Unit = {
    val nullJEnumeration: ju.Enumeration[AnyRef] = null.asInstanceOf[ju.Enumeration[AnyRef]]

    assertNull(nullJEnumeration.asScala)
  }

  @Test def testIterableDecoration(): Unit = {
    val nullJIterable: jl.Iterable[AnyRef] = null.asInstanceOf[jl.Iterable[AnyRef]]

    assertNull(nullJIterable.asScala)
  }

  @Test def testCollectionDecoration(): Unit = {
    val nullJCollection: ju.Collection[AnyRef] = null.asInstanceOf[ju.Collection[AnyRef]]

    assertNull(nullJCollection.asScala)
  }

  @Test def testBufferDecoration(): Unit = {
    val nullJBuffer: ju.List[AnyRef] = null.asInstanceOf[ju.List[AnyRef]]

    assertNull(nullJBuffer.asScala)
  }

  @Test def testSetDecoration(): Unit = {
    val nullJSet: ju.Set[AnyRef] = null.asInstanceOf[ju.Set[AnyRef]]

    assertNull(nullJSet.asScala)
  }

  @Test def testMapDecoration(): Unit = {
    val nullJMap: ju.Map[AnyRef, AnyRef] = null.asInstanceOf[ju.Map[AnyRef, AnyRef]]

    assertNull(nullJMap.asScala)
  }

  @Test def testConcurrentMapDecoration(): Unit = {
    val nullJConMap: juc.ConcurrentMap[AnyRef, AnyRef] = null.asInstanceOf[juc.ConcurrentMap[AnyRef, AnyRef]]

    assertNull(nullJConMap.asScala)
  }

  @Test def testDictionaryDecoration(): Unit = {
    val nullJDict: ju.Dictionary[AnyRef, AnyRef] = null.asInstanceOf[ju.Dictionary[AnyRef, AnyRef]]

    assertNull(nullJDict.asScala)
  }

  @Test def testPropertiesDecoration(): Unit = {
    val nullJProperties: ju.Properties = null.asInstanceOf[ju.Properties]

    assertNull(nullJProperties.asScala)
  }
}
