package dotty.tools.dotc.util

import org.junit.Test
import org.junit.Assert.*

class HashSetTest:

  var counter = 0

  // structural hash and equality, with a counter for ordering
  class Id(val count: Int = { counter += 1; counter }):
    override def hashCode: Int = count
    override def equals(that: Any): Boolean = that match
      case that: Id => this.count == that.count
      case _ => false
    def makeCopy: Id = new Id(count)

  val id1, id2, id3 = Id()

  given Ordering[Id] = Ordering.by(_.count)

  @Test
  def invariant: Unit =
    assert((id1 ne id2) && (id1 ne id3) && (id2 ne id3))
    assert(id1 != id2 && id1 != id3 && id2 != id3)

  @Test
  def newEmpty: Unit =
    val s = HashSet[Id]()
    assert(s.size == 0)
    assert(s.iterator.toList == Nil)

  @Test
  def put: Unit =
    val s = HashSet[Id]()
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
    val s = HashSet[Id]()
    // add id1
    assert(s.size == 0 && !s.contains(id1))
    val added = s.add(id1)
    assert(added && s.size == 1 && s.contains(id1))
    // try add id1 again
    val addedAgain = s.add(id1)
    assert(!addedAgain && s.size == 1 && s.contains(id1)) // no change

  @Test
  def construct: Unit =
    val s = HashSet.from(List(id1,id2,id3))
    assert(s.size == 3)
    assert(s.contains(id1) && s.contains(id2) && s.contains(id3))

  @Test
  def remove: Unit =
    val s = HashSet.from(List(id1,id2,id3))
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
    val s = HashSet.from(List(id1, id2))
    assert(s.lookup(id1) eq id1)
    assert(s.lookup(id2) eq id2)
    assert(s.lookup(id3) eq null)

  @Test
  def iterator: Unit =
    val s = HashSet.from(List(id1,id2,id3))
    assert(s.iterator.toList.sorted == List(id1,id2,id3))

  @Test
  def clear: Unit =
    locally:
      val s1 = HashSet.from(List(id1,id2,id3))
      s1.clear()
      assert(s1.size == 0)
    locally:
      val s2 = HashSet.from(List(id1,id2,id3))
      s2.clear(resetToInitial = false)
      assert(s2.size == 0)

  /** the hash set is based on universal equality, i.e. does not use reference equality */
  @Test
  def universalEquality: Unit =
    val id2_2 = id2.makeCopy

    assert(id2.equals(id2_2)) // structural equality
    assert(id2 ne id2_2) // reference inequality

    val s = HashSet.from(List(id2,id2_2))

    assert(s.size == 1 && s.contains(id2) && s.contains(id2_2))
    assert(s.iterator.toList == List(id2)) // single element
  end universalEquality

