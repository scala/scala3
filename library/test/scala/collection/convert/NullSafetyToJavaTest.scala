package scala.collection.convert

import java.{lang => jl}

import org.junit.Test
import org.junit.Assert.assertNull

import scala.collection.{concurrent, mutable}
import scala.jdk.CollectionConverters.*

// scala/bug#9113: tests to ensure that wrappers return null instead of wrapping it as a collection
class NullSafetyToJavaTest {
  @Test def testIterableWrapping(): Unit = {
    val nullIterable: Iterable[AnyRef] = null.asInstanceOf[Iterable[AnyRef]]
    val iterable: jl.Iterable[AnyRef] = nullIterable.asJava

    assertNull(iterable)
  }

  // Implicit conversion to ju.Properties is not available

  @Test def testIteratorDecoration(): Unit = {
    val nullIterator: Iterator[AnyRef] = null.asInstanceOf[Iterator[AnyRef]]

    assertNull(nullIterator.asJava)
  }

  @Test def testEnumerationDecoration(): Unit = {
    val nullEnumeration: Iterator[AnyRef] = null.asInstanceOf[Iterator[AnyRef]]

    assertNull(nullEnumeration.asJavaEnumeration)
  }

  @Test def testIterableDecoration(): Unit = {
    val nullIterable: Iterable[AnyRef] = null.asInstanceOf[Iterable[AnyRef]]

    assertNull(nullIterable.asJava)
  }

  @Test def testCollectionDecoration(): Unit = {
    val nullCollection: Iterable[AnyRef] = null.asInstanceOf[Iterable[AnyRef]]

    assertNull(nullCollection.asJavaCollection)
  }

  @Test def testBufferDecoration(): Unit = {
    val nullBuffer: mutable.Buffer[AnyRef] = null.asInstanceOf[mutable.Buffer[AnyRef]]

    assertNull(nullBuffer.asJava)
  }

  @Test def testSetDecoration(): Unit = {
    val nullSet: Set[AnyRef] = null.asInstanceOf[Set[AnyRef]]

    assertNull(nullSet.asJava)
  }

  @Test def testMapDecoration(): Unit = {
    val nullMap: mutable.Map[AnyRef, AnyRef] = null.asInstanceOf[mutable.Map[AnyRef, AnyRef]]

    assertNull(nullMap.asJava)
  }

  @Test def testConcurrentMapDecoration(): Unit = {
    val nullConMap: concurrent.Map[AnyRef, AnyRef] = null.asInstanceOf[concurrent.Map[AnyRef, AnyRef]]

    assertNull(nullConMap.asJava)
  }

  @Test def testDictionaryDecoration(): Unit = {
    val nullDict: mutable.Map[AnyRef, AnyRef] = null.asInstanceOf[mutable.Map[AnyRef, AnyRef]]

    assertNull(nullDict.asJavaDictionary)
  }

  // Decorator conversion to ju.Properties is not available
}
