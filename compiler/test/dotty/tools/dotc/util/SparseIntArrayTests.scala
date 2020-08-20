package dotty.tools.dotc.util

import org.junit.Assert._
import org.junit.Test

class SparseIntArrayTests:
  @Test
  def sparseArrayTests: Unit =
    val a = SparseIntArray()
    assert(a.toString == "SparseIntArray()")
    a(1) = 22
    assert(a.toString == "SparseIntArray(1 -> 22)")
    a(222) = 33
    assert(a.toString == "SparseIntArray(1 -> 22, 222 -> 33)")
    a(55555) = 44
    assert(a.toString == "SparseIntArray(1 -> 22, 222 -> 33, 55555 -> 44)")
    assert(a.keysIterator.toList == List(1, 222, 55555))
    assert(a.size == 3, a)
    assert(a.contains(1), a)
    assert(a.contains(222), a)
    assert(a.contains(55555), a)
    assert(!a.contains(2))
    assert(!a.contains(20000000))
    a(222) = 44
    assert(a.size == 3)
    assert(a(1) == 22)
    assert(a(222) == 44)
    assert(a(55555) == 44)
    assert(a.remove(1))
    assert(a.toString == "SparseIntArray(222 -> 44, 55555 -> 44)")
    assert(a(222) == 44, a)
    assert(a.remove(55555))
    assert(a(222) == 44, a)
    assert(a.size == 1)
    assert(!a.contains(1))
    assert(!a.remove(55555))
    assert(a.remove(222))
    assert(a.size == 0)
