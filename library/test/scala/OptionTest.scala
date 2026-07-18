package scala

import org.junit.Assert.*
import org.junit.Test

class OptionTest {
  @Test def testSomeZipSome(): Unit = assertEquals(Some("foo") zip Some("bar"), Some(("foo", "bar")))
  @Test def testSomeZipNone(): Unit = assertEquals(Some("foo") zip None, None)
  @Test def testNoneZipSome(): Unit = assertEquals(None zip Some("bar"), None)
  @Test def testNoneZipNone(): Unit = assertEquals(None zip None, None)

  @Test def testSomeUnzipToSomePair(): Unit = assertEquals(Some(("foo", "bar")).unzip, (Some("foo"), Some("bar")))
  @Test def testSomeUnzipToSomeNone(): Unit = assertEquals(Some(("foo", null)).unzip, (Some("foo"), Some(null)))
  @Test def testNoneUnzipToNonePair(): Unit = assertEquals(None.unzip, (None, None))

  @Test def testSomeUnzip3ToSomeTriple(): Unit = assertEquals(Some(("foo", "bar", "z")).unzip3, (Some("foo"), Some("bar"), Some("z")))
  @Test def testSomeUnzip3ToSomeNone(): Unit = assertEquals(Some(("foo", null, null)).unzip3, (Some("foo"), Some(null), Some(null)))
  @Test def testNoneUnzip3ToNoneTriple(): Unit = assertEquals(None.unzip3, (None, None, None))

  @Test def testSomeZipList(): Unit =
    assertEquals(Iterable(("foo", "bar")), Some("foo").zip(List("bar", "baz")))
  @Test def testSomeZipNil(): Unit =
    assertEquals(Iterable.empty, Some("foo").zip(Nil))
  @Test def testNoneZipList(): Unit =
    assertEquals(Iterable.empty, None.zip(List("bar")))
  @Test def testNoneZipNil(): Unit =
    assertEquals(Iterable.empty, None.zip(Nil))
}
