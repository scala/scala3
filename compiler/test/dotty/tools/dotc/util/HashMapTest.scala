package dotty.tools.dotc.util

import org.junit.Test
import org.junit.Assert.*

class HashMapTest:

  var counter = 0

  // structural hash and equality, but with a counter for ordering
  class Id(val count: Int = { counter += 1; counter }):
    override def hashCode(): Int = count
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
    val m = HashMap[Id, Int]()
    assert(m.size == 0)
    assert(m.iterator.toList == Nil)

  @Test
  def update: Unit =
    val m = HashMap[Id, Int]()
    assert(m.size == 0 && !m.contains(id1))
    m.update(id1, 1)
    assert(m.size == 1 && m(id1) == 1)
    m.update(id1, 2) // replace value
    assert(m.size == 1 && m(id1) == 2)
    m.update(id3, 3) // new key
    assert(m.size == 2 && m(id1) == 2 && m(id3) == 3)

  @Test
  def getOrElseUpdate: Unit =
    val m = HashMap[Id, Int]()
    // add id1
    assert(m.size == 0 && !m.contains(id1))
    val added = m.getOrElseUpdate(id1, 1)
    assert(added == 1 && m.size == 1 && m(id1) == 1)
    // try add id1 again
    val addedAgain = m.getOrElseUpdate(id1, 23)
    assert(addedAgain != 23 && m.size == 1 && m(id1) == 1) // no change

  class StatefulHash:
    var hashCount = 0
    override def hashCode(): Int = { hashCount += 1; super.hashCode() }

  @Test
  def getOrElseUpdate_hashesAtMostOnce: Unit =
    locally:
      val sh1 = StatefulHash()
      val m = HashMap[StatefulHash, Int]() // will be a dense map with default size
      val added = m.getOrElseUpdate(sh1, 1)
      assert(sh1.hashCount == 0) // no hashing at all for dense maps
    locally:
      val sh1 = StatefulHash()
      val m = HashMap[StatefulHash, Int](64) // not dense
      val added = m.getOrElseUpdate(sh1, 1)
      assert(sh1.hashCount == 1) // would be 2 if for example getOrElseUpdate was implemented as lookup + update

  private def fullMap() =
    val m = HashMap[Id, Int]()
    m.update(id1, 1)
    m.update(id2, 2)
    m

  @Test
  def remove: Unit =
    val m = fullMap()
    // remove id2
    m.remove(id2)
    assert(m.size == 1)
    assert(m.contains(id1) && !m.contains(id2))
    // remove id1
    m -= id1
    assert(m.size == 0)
    assert(!m.contains(id1) && !m.contains(id2))

  @Test
  def lookup: Unit =
    val m = fullMap()
    assert(m.lookup(id1) == 1)
    assert(m.lookup(id2) == 2)
    assert(m.lookup(id3) == null)

  @Test
  def iterator: Unit =
    val m = fullMap()
    assert(m.iterator.toList.sorted == List(id1 -> 1,id2 -> 2))

  @Test
  def clear: Unit =
    locally:
      val s1 = fullMap()
      s1.clear()
      assert(s1.size == 0)
    locally:
      val s2 = fullMap()
      s2.clear(resetToInitial = false)
      assert(s2.size == 0)

  // basic structural equality and hash code
  class I32(val x: Int):
    override def hashCode(): Int = x
    override def equals(that: Any): Boolean = that match
      case that: I32 => this.x == that.x
      case _ => false

  /** the hash map is based on universal equality, i.e. does not use reference equality */
  @Test
  def universalEquality: Unit =
    val id2_2 = id2.makeCopy

    assert(id2.equals(id2_2)) // structural equality
    assert(id2 ne id2_2) // reference inequality

    val m = locally:
      val m = HashMap[Id, Int]()
      m(id2) = 23
      m(id2_2) = 29
      m

    assert(m.size == 1 && m(id2) == 29 && m(id2_2) == 29)
    assert(m.keysIterator.toList.head eq id2) // does not replace id2 with id2_2
  end universalEquality

