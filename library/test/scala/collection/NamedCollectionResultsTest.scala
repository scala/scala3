package scala.collection

import org.junit.Test
import org.junit.Assert.assertEquals

import scala.collection.immutable.{HashMap, HashSet, LazyList, LazyListIterable, TreeMap, TreeSet}

class NamedCollectionResultsTest {
  @Test def partitionMapResults(): Unit = {
    def classify(n: Int): Either[Int, String] =
      if (n % 2 == 0) Left(n) else Right(n.toString)

    val iterable = (List(1, 2, 3, 4): Iterable[Int]).partitionMap(classify)
    assertEquals(List(2, 4), iterable.lefts.toList)
    assertEquals(List("1", "3"), iterable.rights.toList)

    val list = List(1, 2, 3, 4).partitionMap(classify)
    assertEquals(List(2, 4), list.lefts.toList)
    assertEquals(List("1", "3"), list.rights.toList)

    val lazyList = LazyList(1, 2, 3, 4).partitionMap(classify)
    assertEquals(List(2, 4), lazyList.lefts.toList)
    assertEquals(List("1", "3"), lazyList.rights.toList)

    val lazyIterable = LazyListIterable(1, 2, 3, 4).partitionMap(classify)
    assertEquals(List(2, 4), lazyIterable.lefts.toList)
    assertEquals(List("1", "3"), lazyIterable.rights.toList)

    val array = Array(1, 2, 3, 4).partitionMap(classify)
    assertEquals(List(2, 4), array.lefts.toList)
    assertEquals(List("1", "3"), array.rights.toList)

    val iarray = IArray(1, 2, 3, 4).partitionMap(classify)
    assertEquals(List(2, 4), iarray.lefts.toList)
    assertEquals(List("1", "3"), iarray.rights.toList)

    val string = "1234".partitionMap(c => if (c % 2 == 0) Left(c) else Right(c))
    assertEquals("24", string.lefts)
    assertEquals("13", string.rights)
  }

  @Test def spanResults(): Unit = {
    val iterable = (List(1, 2, 3, 0): Iterable[Int]).span(_ < 3)
    assertEquals(List(1, 2), iterable.takeWhile.toList)
    assertEquals(List(3, 0), iterable.dropWhile.toList)

    val list = List(1, 2, 3, 0).span(_ < 3)
    assertEquals(List(1, 2), list.takeWhile.toList)
    assertEquals(List(3, 0), list.dropWhile.toList)

    val vector = Vector(1, 2, 3, 0).span(_ < 3)
    assertEquals(List(1, 2), vector.takeWhile.toList)
    assertEquals(List(3, 0), vector.dropWhile.toList)

    val array = Array(1, 2, 3, 0).span(_ < 3)
    assertEquals(List(1, 2), array.takeWhile.toList)
    assertEquals(List(3, 0), array.dropWhile.toList)

    val iarray = IArray(1, 2, 3, 0).span(_ < 3)
    assertEquals(List(1, 2), iarray.takeWhile.toList)
    assertEquals(List(3, 0), iarray.dropWhile.toList)

    val iterator = Iterator(1, 2, 3, 0).span(_ < 3)
    assertEquals(List(1, 2), iterator.takeWhile.toList)
    assertEquals(List(3, 0), iterator.dropWhile.toList)

    val range = (1 to 4).span(_ < 3)
    assertEquals(List(1, 2), range.takeWhile.toList)
    assertEquals(List(3, 4), range.dropWhile.toList)

    val treeSet = TreeSet(1, 2, 3, 4).span(_ < 3)
    assertEquals(List(1, 2), treeSet.takeWhile.toList)
    assertEquals(List(3, 4), treeSet.dropWhile.toList)

    val string = "1230".span(_ < '3')
    assertEquals("12", string.takeWhile)
    assertEquals("30", string.dropWhile)

    val treeMap = TreeMap(1 -> "a", 2 -> "b", 3 -> "c").span(_._1 < 3)
    assertEquals(TreeMap(1 -> "a", 2 -> "b"), treeMap.takeWhile)
    assertEquals(TreeMap(3 -> "c"), treeMap.dropWhile)

    val hashMap = HashMap(1 -> "a", 2 -> "b").span(_ => false)
    assertEquals(HashMap.empty[Int, String], hashMap.takeWhile)
    assertEquals(HashMap(1 -> "a", 2 -> "b"), hashMap.dropWhile)

    val hashSet = HashSet(1, 2, 3).span(_ => false)
    assertEquals(HashSet.empty[Int], hashSet.takeWhile)
    assertEquals(HashSet(1, 2, 3), hashSet.dropWhile)
  }

  @Test def splitAtResults(): Unit = {
    val iterable = (List(1, 2, 3, 4): Iterable[Int]).splitAt(2)
    assertEquals(List(1, 2), iterable.taken.toList)
    assertEquals(List(3, 4), iterable.dropped.toList)

    val list = List(1, 2, 3, 4).splitAt(2)
    assertEquals(List(1, 2), list.taken.toList)
    assertEquals(List(3, 4), list.dropped.toList)

    val array = Array(1, 2, 3, 4).splitAt(2)
    assertEquals(List(1, 2), array.taken.toList)
    assertEquals(List(3, 4), array.dropped.toList)

    val iarray = IArray(1, 2, 3, 4).splitAt(2)
    assertEquals(List(1, 2), iarray.taken.toList)
    assertEquals(List(3, 4), iarray.dropped.toList)

    val iterator = Iterator(1, 2, 3, 4).splitAt(2)
    assertEquals(List(1, 2), iterator.taken.toList)
    assertEquals(List(3, 4), iterator.dropped.toList)

    val range = (1 to 4).splitAt(2)
    assertEquals(List(1, 2), range.taken.toList)
    assertEquals(List(3, 4), range.dropped.toList)

    val numericRange = (BigInt(1) to BigInt(4)).splitAt(2)
    assertEquals(List(BigInt(1), BigInt(2)), numericRange.taken.toList)
    assertEquals(List(BigInt(3), BigInt(4)), numericRange.dropped.toList)

    val string = "1234".splitAt(2)
    assertEquals("12", string.taken)
    assertEquals("34", string.dropped)
  }
}
