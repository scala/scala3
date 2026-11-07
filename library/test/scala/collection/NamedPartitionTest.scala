package scala.collection

import org.junit.Test
import org.junit.Assert.assertEquals

import scala.collection.immutable.{BitSet, HashMap, HashSet, LazyList, LazyListIterable, Stream, TreeMap, TreeSet}

class NamedPartitionTest {
  private def check(passed: Iterable[Int], rejected: Iterable[Int]): Unit = {
    assertEquals(List(2, 4), passed.toList.sorted)
    assertEquals(List(1, 3), rejected.toList.sorted)
  }

  @Test def iterablePartitions(): Unit = {
    val xs: Iterable[Int] = List(1, 2, 3, 4)
    val iterable = xs.partition(_ % 2 == 0)
    check(iterable.passed, iterable.rejected)

    val list = List(1, 2, 3, 4).partition(_ % 2 == 0)
    check(list.passed, list.rejected)

    val vector = Vector(1, 2, 3, 4).partition(_ % 2 == 0)
    check(vector.passed, vector.rejected)

    val view = xs.view.partition(_ % 2 == 0)
    check(view.passed, view.rejected)

    val lazyList = LazyList(1, 2, 3, 4).partition(_ % 2 == 0)
    check(lazyList.passed, lazyList.rejected)

    val lazyIterable = LazyListIterable(1, 2, 3, 4).partition(_ % 2 == 0)
    check(lazyIterable.passed, lazyIterable.rejected)
  }

  @deprecated("Tests deprecated Stream", since="2.13")
  @Test def streamPartition(): Unit = {
    val stream = Stream(1, 2, 3, 4).partition(_ % 2 == 0)
    check(stream.passed, stream.rejected)
  }

  @Test def setPartitions(): Unit = {
    val hashSet = HashSet(1, 2, 3, 4).partition(_ % 2 == 0)
    check(hashSet.passed, hashSet.rejected)

    val treeSet = TreeSet(1, 2, 3, 4).partition(_ % 2 == 0)
    check(treeSet.passed, treeSet.rejected)

    val bitSet = BitSet(1, 2, 3, 4).partition(_ % 2 == 0)
    check(bitSet.passed, bitSet.rejected)

    val mutableBitSet = mutable.BitSet(1, 2, 3, 4).partition(_ % 2 == 0)
    check(mutableBitSet.passed, mutableBitSet.rejected)
  }

  @Test def mapPartitions(): Unit = {
    val hashMap = HashMap(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d").partition(_._1 % 2 == 0)
    check(hashMap.passed.keys, hashMap.rejected.keys)

    val treeMap = TreeMap(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d").partition(_._1 % 2 == 0)
    check(treeMap.passed.keys, treeMap.rejected.keys)

    val view = Map(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d").view.partition(_._1 % 2 == 0)
    check(view.passed.keys, view.rejected.keys)

    val empty = MapView.empty[Int, String].partition(_ => true)
    assertEquals(Map.empty[Int, String], empty.passed.toMap)
    assertEquals(Map.empty[Int, String], empty.rejected.toMap)
  }

  @Test def arrayStringAndIteratorPartitions(): Unit = {
    val array = Array(1, 2, 3, 4).partition(_ % 2 == 0)
    check(array.passed.toList, array.rejected.toList)

    val iarray = IArray(1, 2, 3, 4).partition(_ % 2 == 0)
    check(iarray.passed.toList, iarray.rejected.toList)

    val string = "1234".partition(_ % 2 == 0)
    assertEquals("24", string.passed)
    assertEquals("13", string.rejected)

    val iterator = Iterator(1, 2, 3, 4).partition(_ % 2 == 0)
    check(iterator.passed.toList, iterator.rejected.toList)
  }

  @Test def ordinaryTupleUsage(): Unit = {
    val result = List(1, 2, 3, 4).partition(_ % 2 == 0)
    val pair: (List[Int], List[Int]) = result
    val (passed, rejected) = result
    assertEquals((List(2, 4), List(1, 3)), pair)
    check(passed, rejected)
    assertEquals(result.passed, result.toTuple._1)
    assertEquals(result.rejected, result.toTuple._2)

    val empty = List.empty[Int].partition(_ => true)
    assertEquals(List.empty[Int], empty.passed)
    assertEquals(List.empty[Int], empty.rejected)

    val all = List(1, 3).partition(_ % 2 == 1)
    assertEquals(List(1, 3), all.passed)
    assertEquals(List.empty[Int], all.rejected)

    val none = List(1, 3).partition(_ % 2 == 0)
    assertEquals(List.empty[Int], none.passed)
    assertEquals(List(1, 3), none.rejected)
  }
}
