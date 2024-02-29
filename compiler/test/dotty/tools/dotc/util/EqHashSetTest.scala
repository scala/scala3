package dotty.tools.dotc.util

import org.junit.Test
import org.junit.Assert.*

class EqHashSetTest:

  var counter = 0

  // basic identity hash, and reference equality, but with a counter for ordering
  class Id:
    val count = { counter += 1; counter }

  val id1, id2, id3 = Id()

  given Ordering[Id] = Ordering.by(_.count)

  @Test
  def invariant: Unit =
    assert((id1 ne id2) && (id1 ne id3) && (id2 ne id3))

  @Test
  def newEmpty: Unit =
    val s = EqHashSet[Id]()
    assert(s.size == 0)
    assert(s.iterator.toList == Nil)

  @Test
  def put: Unit =
    val s = EqHashSet[Id]()
    // put id1
    assert(s.size == 0 && !s.contains(id1))
    s += id1
    assert(s.size == 1 && s.contains(id1))
    // put id2
    assert(!s.contains(id2))
    s.put(id2)
    assert(s.size == 2 && s.contains(id1) && s.contains(id2))
    // put id3
    s ++= List(id3)
    assert(s.size == 3 && s.contains(id1) && s.contains(id2) && s.contains(id3))

  @Test
  def add: Unit =
    val s = EqHashSet[Id]()
    // add id1
    assert(s.size == 0 && !s.contains(id1))
    val added = s.add(id1)
    assert(added && s.size == 1 && s.contains(id1))
    // try add id1 again
    val addedAgain = s.add(id1)
    assert(!addedAgain && s.size == 1 && s.contains(id1)) // no change

  @Test
  def construct: Unit =
    val s = EqHashSet.from(List(id1,id2,id3))
    assert(s.size == 3)
    assert(s.contains(id1) && s.contains(id2) && s.contains(id3))

  @Test
  def remove: Unit =
    val s = EqHashSet.from(List(id1,id2,id3))
    // remove id2
    s.remove(id2)
    assert(s.size == 2)
    assert(s.contains(id1) && !s.contains(id2) && s.contains(id3))
    // remove id1
    s -= id1
    assert(s.size == 1)
    assert(!s.contains(id1) && !s.contains(id2) && s.contains(id3))
    // remove id3
    s --= List(id3)
    assert(s.size == 0)
    assert(!s.contains(id1) && !s.contains(id2) && !s.contains(id3))

  @Test
  def lookup: Unit =
    val s = EqHashSet.from(List(id1, id2))
    assert(s.lookup(id1) eq id1)
    assert(s.lookup(id2) eq id2)
    assert(s.lookup(id3) eq null)

  @Test
  def iterator: Unit =
    val s = EqHashSet.from(List(id1,id2,id3))
    assert(s.iterator.toList.sorted == List(id1,id2,id3))

  @Test
  def clear: Unit =
    locally:
      val s1 = EqHashSet.from(List(id1,id2,id3))
      s1.clear()
      assert(s1.size == 0)
    locally:
      val s2 = EqHashSet.from(List(id1,id2,id3))
      s2.clear(resetToInitial = false)
      assert(s2.size == 0)

  // basic structural equality and hash code
  class I32(val x: Int):
    override def hashCode(): Int = x
    override def equals(that: Any): Boolean = that match
      case that: I32 => this.x == that.x
      case _ => false

  /** the hash map is based on reference equality, i.e. does not use universal equality */
  @Test
  def referenceEquality: Unit =
    val i1, i2 = I32(1) // different instances

    assert(i1.equals(i2)) // structural equality
    assert(i1 ne i2) // reference inequality

    val s = EqHashSet.from(List(i1,i2))

    assert(s.size == 2 && s.contains(i1) && s.contains(i2))
    assert(s.iterator.toSet == Set(i1)) // scala.Set delegates to universal equality
  end referenceEquality

